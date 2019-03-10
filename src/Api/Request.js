

exports.json2FormData = function (json) {
  var formData = new FormData();
  console.log(json)
  for (var key in json) {
    formData.append(key, json[key]);
  }
  console.log(formData)
  console.log(formData.get("phone"))
  return formData;
}

exports.json2String = function (json) {
  var result = [];
  for (var key in json) {
    result.push(key + "=" + json[key]);
  }
  return result.join("&")
}