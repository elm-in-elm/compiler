const registerPort = (app, portName, callback) => {
  if (app.ports && app.ports[portName]) {
    app.ports[portName].subscribe(callback);
  } else {
    console.log(`Tried to register an Elm port callback but failed: ${portName}`);
  }
};

module.exports = {
  registerPort
};
