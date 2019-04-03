exports.openLoginModal = function () {
  $("#loginModal").modal("show");
}

exports.openRegisterModal = function (str) {
  return function () {
    $("#registerModal").modal("show");
    document.getElementById("validation-inviter").value = str;
  }
}

exports.closeLoginModal = function () {
  $("#loginModal").modal("hide");
}

exports.closeRegisterModal = function () {
  $("#registerModal").modal("hide");
}

exports.openBindAlipayModal = function () {
  $('#bindAlipayModal').modal({
    backdrop: 'static',
    keyboard: false
})
}

exports.closeBindAlipayModal = function () {
  $("#bindAlipayModal").modal("hide");
}

exports.getInputValue = function (inputId) {
  return function () {
    var input = document.getElementById(inputId);
    if (input != undefined && input.value != undefined ) {
        return input.value;
    } else {
      return ""
    }
  }
}

exports.getRadioBtnChecked = function (id) {
  return function () {
    var radioBtn = document.getElementById(id);
    if (radioBtn != undefined && radioBtn.checked != undefined ) {
      return radioBtn.checked;
    } else {
      return false
    }
  }
}

exports.callback = function (func) {
  func();
}

exports.alertMsg = function (msg) {
  return function () {
    window.alert(msg);
  }
}


var commodities = {};
exports.initCommodities = function () {
  sendHttpRequest("GET", "/public/commodity/", "").then(function (result) {
    for (var i = 0; i < result.length; ++i) {
      commodities[result[i].id] = result[i];
    }
  });
}

exports.getCommodityById = function (id) {
  // return function () {
    var result =  commodities[id];
    if (result ) {
      return result;
    } else {
      return { name : "已下架"} 
    }
  // }
}

var baseUrl = "http://www.scix.vip/yzmall-server";

function sendHttpRequest(type, uri, body) {
  return new Promise(function (resolve, reject) {
      var xhr = new XMLHttpRequest();
      xhr.withCredentials = true;
      xhr.onload = function () {
          if ('' !== xhr.response && xhr.status == 200) {
              resolve(JSON.parse(xhr.response));
          } else {
              reject(Error("Network Error"));
          }
      };
      xhr.onerror = function() {
          reject(Error("Network Error"));
      }
      if (type == "POST") {
          xhr.open(type, "http://www.scix.vip/yzmall-server" + uri, true);
          xhr.setRequestHeader("Content-Type", "application/x-www-form-urlencoded");
          xhr.send(body);
      } else if (type == "GET") {
          xhr.open(type, "http://www.scix.vip/yzmall-server" + uri, true);
          xhr.send(null);
      } else if ("POST_JSON" == type) {
          xhr.open("POST", "http://www.scix.vip/yzmall-server" + uri, true);
          xhr.setRequestHeader("Content-Type", "application/json");
          xhr.send(JSON.stringify(body));
      }
  });
}


exports.getInviterId = function () {
  var href = window.location.href;
  var index = href.indexOf('inviter_id=');
  if (index == -1) {
    return "";
  } else {
    return href.slice(index + 11);
  }
}


exports.showProv = function () {
    initMethod01();
    var len = provice.length;
    for (var i = 0; i < len; i++) {
        var provOpt = document.createElement('option');
        provOpt.innerText = provice[i]['name'];
        provOpt.value = i;
        prov.appendChild(provOpt);
    }
}

var _value = 0;
exports.getTimerCurrent = function () {
  return _value;
}

exports.timerDown = function () {
  _value = _value - 1;
}

exports.setTimer = function (v) {
  _value = v;
}

function setInputFilter(textbox, inputFilter) {
  ["input", "keydown", "keyup", "mousedown", "mouseup", "select", "contextmenu", "drop"].forEach(function(event) {
    if (!textbox) {
      return;
    }
    textbox.addEventListener(event, function() {
      if (inputFilter(this.value)) {
        this.oldValue = this.value;
        this.oldSelectionStart = this.selectionStart;
        this.oldSelectionEnd = this.selectionEnd;
      } else if (this.hasOwnProperty("oldValue")) {
        this.value = this.oldValue;
        this.setSelectionRange(this.oldSelectionStart, this.oldSelectionEnd);
      }
    });
  });
}


exports.setNumOnly = function (id) {
  return function () {
    setInputFilter(document.getElementById(id), function(value) {
      return /^\d*$/.test(value); 
    });
  }
}