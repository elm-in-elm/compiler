const registerPort = (app, portName, callback) => {
  if (app.ports && app.ports[portName]) {
    app.ports[portName].subscribe(callback);
  }
};

module.exports = {
  registerPort
};
