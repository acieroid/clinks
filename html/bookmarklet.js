server_url = "";

function getval(id) {
    elem = document.getElementById(id);
    return elem.value;
}

function getcookie(name) {
    return "TODO";
}

function message(str) {
    document.getElementById("clinks_message").innerHTML = '<b>' + str + '</b>';
}

function message_from_response(status, str) {
    var parser = new DOMParser();
    var dom = parser.parseFromString(str, "text/xml");
    message(dom.firstChild.firstChild.data);
}

function save() {
    var link = new Clinks.Link(getval("clinks_url"), getval("clinks_title"),
                               getval("clinks_tags"), getval("clinks_notes"),
                               getcookie("username"), getcookie("password"));
    message("Sending...");
    link.onresponse = message_from_response;
    link.create(server_url);
}
    
container = document.createElement("div");
container.style.textAlign = "center";
div = document.createElement("div");
div.style.display = "inline-block";
div.style.backgroundColor = "#abf";
div.style.padding = "10px";

div.innerHTML = '<div id="clinks_message"></div>' +
    '<label for="clinks_url">URL: </label>' +
    '<input type="text" id="clinks_url"/><br/>' +
    '<label for="clinks_title">Title: </label>' +
    '<input type="text" id="clinks_title"/><br/>' +
    '<label for="clinks_tags">Tags: </label>' +
    '<input type="text" id="clinks_tags"/><br/>' +
    '<textarea id="clinks_notes"></textarea><br/>' +
    '<input type="button" id="clinks_save" value="Save" onclick="save()"/>';

container.appendChild(div);
body = document.getElementsByTagName("body")[0];
body.appendChild(container);

/* Bookmarklet:
javascript:var%20server="http://localhost:8080/";function%20load(src){var%20head=document.getElementsByTagName("head")[0],script=document.createElement("script");script.type="text/javascript";script.src=server+src;head.appendChild(script);}load("clinks.js");load("bookmarklet.js");
*/