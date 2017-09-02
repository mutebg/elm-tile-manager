const functions = require("firebase-functions");
const admin = require("firebase-admin");
const express = require("express");
const cors = require("cors");
const Multer = require("multer");
const gcloud = require("google-cloud")({
  projectId: "elm-receipts",
  keyFilename: "firebase-key.json"
});
const uuidv4 = require("uuid/v4");
const fs = require("fs");
const bodyParser = require("body-parser");

const storage = gcloud.storage();
const vision = gcloud.vision();
const app = express();

const CLOUD_BUCKET = "elm-receipts.appspot.com";

const multer = Multer({
  storage: Multer.MemoryStorage,
  fileSize: 5 * 1024 * 1024
});

admin.initializeApp(functions.config().firebase);

// Express middleware that validates Firebase ID Tokens passed in the Authorization HTTP header.
// The Firebase ID token needs to be passed as a Bearer token in the Authorization HTTP header like this:
// `Authorization: Bearer <Firebase ID Token>`.
// when decoded successfully, the ID Token content will be added as `req.user`.
const authenticate = (req, res, next) => {
  if (
    !req.headers.authorization ||
    !req.headers.authorization.startsWith("Bearer ")
  ) {
    res.status(403).send("Unauthorized");
    return;
  }
  const idToken = req.headers.authorization.split("Bearer ")[1];
  admin
    .auth()
    .verifyIdToken(idToken)
    .then(decodedIdToken => {
      req.user = decodedIdToken;
      next();
    })
    .catch(error => {
      res.status(403).send("Unauthorized");
    });
};

app.use(cors());
//app.use(authenticate);
//app.use(bodyParser.json()); // for parsing application/json
//app.use(bodyParser.urlencoded({ extended: true })); // for parsing application/x-www-form-urlencoded

// POST /api/receipts
// Create a new receipts
app.post("/tiles", (req, res) => {
  const id = req.body.id;
  const name = req.body.name;
  const image_url = req.body.image_url;
  const type_ = req.body.type_;
  const action = req.body.action;
  const active = req.body.active;
  const modified = new Date() + "";
  const data = { name, image_url, type_, action, active, modified };

  if (!id) {
    console.log("CREAT");
    data.created = new Date() + "";
    console.log(data);
    admin
      .database()
      .ref(`/tiles`)
      .push(data)
      .then(snapshot => {
        return snapshot.ref.once("value");
      })
      .then(snapshot => {
        const val = Object.assign({}, { id: snapshot.key }, snapshot.val());
        res.status(201).json(val);
      })
      .catch(error => {
        console.log("Error detecting sentiment or saving Tiles", error.message);
        res.sendStatus(500);
      });
  } else {
    console.log("UPDATE");
    //res.status(201).json(data);
    res.sendStatus(500);
  }
});

// Get all receipts
app.get("/tiles", (req, res) => {
  let query = admin.database().ref(`/tiles`);
  query
    .once("value")
    .then(snapshot => {
      var receipts = [];
      snapshot.forEach(childSnapshot => {
        receipts.push(
          Object.assign({}, { id: childSnapshot.key }, childSnapshot.val())
        );
      });

      return res.status(200).json(receipts);
    })
    .catch(error => {
      console.log("Error getting Tiles", error.message);
      res.sendStatus(500);
    });
});

// Delete recept
app.delete("/tiles/:id", (req, res) => {
  admin
    .database()
    .ref(`/tiles/${req.params.id}`)
    .remove()
    .then(() => {
      return res
        .status(200)
        .json({ code: 0, message: "Tile has been deleted" });
    })
    .catch(error => {
      console.log("Error deleting Tile", id, error.message);
      res.sendStatus(500);
    });
});

// Upload receipt image
app.post("/upload", multer.any(), (req, res) => {
  const file = req.files[0];
  const bucket = storage.bucket(CLOUD_BUCKET);
  const gcsname = uuidv4();
  const files = bucket.file(gcsname);

  const stream = files.createWriteStream({
    metadata: {
      contentType: file.mimetype
    }
  });

  stream.on("error", err => {
    console.log(err);
  });

  stream.on("finish", () => {
    files.makePublic().then(() => {
      const fileUrl = `https://storage.googleapis.com/${CLOUD_BUCKET}/${gcsname}`;

      vision.readDocument(fileUrl, (err, text, apiResponse) => {
        if (err) {
          console.log("err", err);
        }
        console.log("done");
        // text = 'This paragraph was extracted from image.jpg';
        res.json({
          //text: text,
          amount: 4.15,
          fileUrl: fileUrl
        });
      });
    });
  });

  stream.end(file.buffer);
});

// Expose the API as a function
exports.api = functions.https.onRequest(app);
