const registerPort = (app, portName, callback) => {
  if (app.ports && app.ports[portName]) {
    app.ports[portName].subscribe(callback);
  } else {
    console.error(`Tried to register an Elm port callback but failed: ${portName}. Most likely because the port is unused on the Elm side.`);
    //process.exit(1);
  }
};

module.exports = {
  registerPort
};
