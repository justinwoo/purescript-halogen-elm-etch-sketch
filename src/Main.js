exports.getElmInstance = function (element) {
  return function () {
    return window.Elm.Main.embed(element);
  }
}

exports.subscribeToClearScreen_ = function (instance) {
  return function (push) {
    return function () {
      instance.ports.clearScreen.subscribe(function () {
        push()()
      })
    }
  }
}

exports.sendModelUpdate = function (instance) {
  return function (model) {
    return function () {
      instance.ports.modelUpdates.send(model);
    }
  }
}
