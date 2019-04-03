

var baseUrl = 'http://192.168.0.138:8080/yzmall-server'
var editUri = '';
var fields = [];
var accountId = 0;

function initEvents () {
    $("#loginBtn").click(async function () {
        let password = document.getElementById("password").value;
        let phone = document.getElementById("phone").value;
        await login(phone, password)
    });
    $('#edit_config').click(switchConfig);
    $('#edit_account').click(switchAccount);
    $('#add_commodity').click(addCommodity);
    $('#edit_commodity').click(editCommodity);
    $('#edit_order').click(editOrder);
}

async function login(phone, password) {
    await post(`/public/account/login`, `phone=${phone}&password=${password}`);
}

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
            xhr.open(type, baseUrl + uri, true);
            xhr.setRequestHeader("Content-Type", "application/x-www-form-urlencoded");
            xhr.send(body);
        } else if (type == "GET") {
            xhr.open(type, baseUrl + uri, true);
            xhr.send(null);
        } else if ("POST_JSON" == type) {
            xhr.open("POST", baseUrl + uri, true);
            xhr.setRequestHeader("Content-Type", "application/json");
            xhr.send(JSON.stringify(body));
        }
    });
}


function get(uri) {
    return sendHttpRequest("GET", uri, null);
}

function post(uri, body) {
    return sendHttpRequest("POST", uri, body);
}

function post_json(uri, body) {
    return sendHttpRequest("POST_JSON", uri, body);
}

async function tryLogin() {
    try {
        let result = await get('/account/mine');
        return true;
   } catch (e) {
       return false;
   }
}



async function postEditResult() {
    let json = {};
    fields.forEach(field => {
        json[field] = document.getElementById(field).value;
    })
    let result = await post_json(editUri, json);
    callback(result);
}

var callback = (sth) => {};

/**
 * render Config
 */

function renderTh(name) {
    return `<th scope="col">${name}</th>`
}

function renderThead(names) {
    return `<thead><tr>${names.map(renderTh).join("")}<th scope="col">操作</th></tr></thead>`
}

function renderTBody(names) {
    return (record) => {
        let func = (name, index) => {
            if (index == 0) {
                return `<th scope="row">${record[name]}</th>`
            } else {
                return `<td>${record[name]}</td>`;
            }
        };
        let content = names.map(func).join("") 
                    + `<td><button class="btn btn-primary" id="popEditorBtn">修改</button>`; 
        return `<tbody><tr>${content}</tr></tbody>`;
    }
}


function renderTable(records, names, feilds) {
    return `<table class="table table-striped">`
            + renderThead(names) 
            + records.map(renderTBody(feilds)).join("")
            + "</table>";
}

// ---------------

function renderModalForm (field, name, value) {
    return `
    <div class="form-group row mx-2">
        <label for="${field}" class="col-form-label col-sm-4">${name}</label>
        <input type="text" class="form-control col-sm-8" id="${field}" value="${value}">
    </div>
    `
}

function renderModal (record, names, feilds) {
    let content = `
    <div class="modal-dialog modal-lg" role="document">
        <div class="modal-content">
            <div class="modal-header">
                <h5 class="modal-title" id="exampleModalLabel">编辑</h5>
                <button type="button" class="close" data-dismiss="modal" aria-label="Close">
                    <span aria-hidden="true">&times;</span>
                </button>
            </div>
            <div class="modal-body">
                ${feilds.map((field,index) => renderModalForm(field, names[index], record[field])).join("")}
            </div>
            <div class="modal-footer">
                <button type="button" class="btn btn-secondary" data-dismiss="modal">关闭</button>
                <button type="button" class="btn btn-primary" id="editBtn" data-dismiss="modal">保存</button>
            </div>
        </div>
    </div>`
    document.getElementById('editorModal').innerHTML = content;
}

function showError(msg) {
    let content = `<div class="alert alert-danger" role="alert">
        ${msg}
        </div>
    `;
    document.getElementById('content-container').innerHTML = content;
}


async function switchConfig() {
    let config = await get('/management/config');
    renderConfig(config);
    fields = configFields;
    editUri = '/management/config';
    $('#popEditorBtn').click(function () {
        $('#editorModal').modal('show');
    });
    $('#editBtn').click(postEditResult);
    callback = renderConfig;
}

async function renderConfig(config) {
    document.getElementById('content-container').innerHTML = renderTable([ config ], configNames, configFields);
    renderModal(config, configNames, configFields);
}



//------------------
// account 
async function switchAccount() {
    let accountId = document.getElementById('edit_account_input').value;
    let account = await get(`/management/account/${accountId}`);
    renderAccount(account);
    fields = accountFields;
    editUri = `/management/account/${accountId}`;
    $('#popEditorBtn').click(function () {
        $('#editorModal').modal('show');
    });
    $('#editBtn').click(postEditResult);
    callback = renderAccount;
}

async function renderAccount(account) {
    document.getElementById('content-container').innerHTML = renderTable([ account ], accountNames, accountFields);
    renderModal(account, accountNames, accountFields);
}



// add commodity
async function addCommodity() {
    let commodity = {};
    commodityFields.forEach(field => {
        commodity[field] = "";
    })
    renderModal(commodity, commodityNames, commodityFields);
    
    fields = commodityFields;
    editUri = `/management/account/${accountId}`;

    $('#editorModal').modal('show');

    $('#editBtn').click(postEditResult);
    callback = renderAccount;
}


async function editCommodity() {
    let commodityId = document.getElementById('edit_commodity_input').value;
    let commodity = await get(`	/public/commodity/${commodityId}`);
    renderCommodity(commodity);
    fields = commodityFields;
    editUri = `/management/commodity/${commodityId}`;
    $('#popEditorBtn').click(function () {
        $('#editorModal').modal('show');
    });
    $('#editBtn').click(postEditResult);
    callback = renderCommodity;
}

async function renderCommodity(commodity) {
    document.getElementById('content-container').innerHTML = renderTable([ commodity ], commodityNames, commodityFields);
    renderModal(commodity, commodityNames, commodityFields);
}

//
async function editOrder() {
    let orderId = document.getElementById('edit_order_input').value;
    let order = await get(`/management/order/${orderId}`);
    renderOrder(order);
    fields = orderFields;
    editUri = `/management/order/${orderId}`;
    $('#popEditorBtn').click(function () {
        $('#editorModal').modal('show');
    });
    $('#editBtn').click(postEditResult);
    callback = renderOrder;
}

async function renderOrder(order) {
    document.getElementById('content-container').innerHTML = renderTable([ order ], orderNames, orderFields);
    renderModal(order, orderNames, orderFields);
}


async function editactSharedRecord() {
    let actSharedRecordId = document.getElementById('edit_mytsharedrecord_input').value;
    let actSharedRecord = await get(`/management/actSharedRecord/${actSharedRecordId}`);
    renderActSharedRecord(actSharedRecord);
    fields = actSharedRecordFields;
    editUri = `/management/actSharedRecord/${actSharedRecordId}`;
    $('#popEditorBtn').click(function () {
        $('#editorModal').modal('show');
    });
    $('#editBtn').click(postEditResult);
    callback = renderActSharedRecord;
}

async function renderActSharedRecord(actSharedRecord) {
    document.getElementById('content-container').innerHTML = renderTable([ actSharedRecord ], actSharedRecordNames, actSharedRecordFields);
    renderModal(actSharedRecord, actSharedRecordNames, actSharedRecordFields);
}