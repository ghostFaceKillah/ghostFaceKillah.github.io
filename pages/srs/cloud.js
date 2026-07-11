// Cloud sync for the SRS app: Google sign-in + Firestore (see DESIGN.md).
//
// Loaded as <script type="module"> — it pulls the Firebase SDK from the
// gstatic CDN, then publishes window.SRS_CLOUD and dispatches "srs-cloud-ready"
// on window. If the SDK can't load (offline, blocked), nothing is published
// and the app simply stays in guest mode; every app feature works without
// this file succeeding.
//
// Data model (guarded by /firestore.rules — each user owns users/{uid}/**):
//   users/{uid}/cards/{docId}     — one doc per card: the FSRS state
//   users/{uid}/settings/app      — { groups, newPerDay }
//
// Card IDs contain "/" (e.g. "words/L1D1/你"), which Firestore forbids in
// document IDs, so doc IDs store them with "/" swapped for "|". The mapping
// must never change: review history is keyed by it.
//
// Sync contract with the app (index.html):
//   - reviews NEVER block on the network: pushCard/pushSettings are
//     fire-and-forget; Firestore's IndexedDB persistence queues writes
//     offline and replays them when connectivity returns.
//   - fetchAll() is called once per sign-in to hydrate local state.
//   - importAll() is only called from the explicit one-time import flow.

const CDN = "https://www.gstatic.com/firebasejs/11.6.0";

const encodeId = id => id.replaceAll("/", "|");
const decodeId = docId => docId.replaceAll("|", "/");

async function boot() {
  const config = window.FIREBASE_CONFIG;
  if (!config) return; // config file missing — guest mode only

  const [{ initializeApp }, authMod, fsMod] = await Promise.all([
    import(`${CDN}/firebase-app.js`),
    import(`${CDN}/firebase-auth.js`),
    import(`${CDN}/firebase-firestore.js`),
  ]);
  const {
    getAuth, onAuthStateChanged, signInWithPopup, GoogleAuthProvider, signOut,
  } = authMod;
  const {
    initializeFirestore, persistentLocalCache,
    doc, setDoc, getDoc, getDocs, collection, writeBatch,
  } = fsMod;

  const app = initializeApp(config);
  const auth = getAuth(app);
  let db;
  try {
    db = initializeFirestore(app, { localCache: persistentLocalCache() });
  } catch (e) {
    // IndexedDB unavailable (private browsing etc.) — fall back to memory cache
    db = initializeFirestore(app, {});
  }

  let user = undefined; // undefined until onAuthStateChanged resolves once
  const listeners = [];

  const cardDoc = id => doc(db, "users", user.uid, "cards", encodeId(id));
  const settingsDoc = () => doc(db, "users", user.uid, "settings", "app");

  window.SRS_CLOUD = {
    get user() { return user; },

    onUserChange(cb) { listeners.push(cb); if (user !== undefined) cb(user); },

    async signIn() {
      await signInWithPopup(auth, new GoogleAuthProvider());
    },

    async signOut() {
      await signOut(auth);
    },

    // Fire-and-forget: errors are logged, never thrown at the review flow.
    pushCard(id, state) {
      if (!user) return;
      setDoc(cardDoc(id), state).catch(e => console.warn("srs sync: card push failed", e));
    },

    pushSettings(settings) {
      if (!user) return;
      setDoc(settingsDoc(), settings).catch(e => console.warn("srs sync: settings push failed", e));
    },

    // One-shot hydration after sign-in → { cards: {cardId: state}, settings|null }.
    async fetchAll() {
      if (!user) return null;
      const [cardsSnap, settingsSnap] = await Promise.all([
        getDocs(collection(db, "users", user.uid, "cards")),
        getDoc(settingsDoc()),
      ]);
      const cards = {};
      cardsSnap.forEach(d => { cards[decodeId(d.id)] = d.data(); });
      return { cards, settings: settingsSnap.exists() ? settingsSnap.data() : null };
    },

    // Explicit guest-progress import: batched writes (Firestore caps at 500/batch).
    async importAll(cards, settings) {
      if (!user) return;
      const ids = Object.keys(cards);
      for (let i = 0; i < ids.length; i += 450) {
        const batch = writeBatch(db);
        for (const id of ids.slice(i, i + 450)) batch.set(cardDoc(id), cards[id]);
        if (i === 0 && settings) batch.set(settingsDoc(), settings);
        await batch.commit();
      }
    },
  };

  onAuthStateChanged(auth, u => {
    user = u;
    for (const cb of listeners) cb(u);
  });

  window.dispatchEvent(new CustomEvent("srs-cloud-ready"));
}

boot().catch(e => console.warn("srs sync: cloud unavailable, staying in guest mode", e));
