/* TODO: use cookies */
var connected = false;
var timeout = false;
var server_url = ""; /* on the same host */

function message(str) {
    $("#message").show();
    $("#message").text(str);
    if (timeout) {
        clearTimeout(timeout);
        timeout = false;
    }
    timeout = setTimeout(function () { $("#message").hide(); }, 3000);
}

function message_from_response(status, str) {
    var xmlDoc = $.parseXML(str);
    var xml = $(xmlDoc);
    /* only one of these two fields exists */
    message(xml.find("error").text() + xml.find("success").text());
}

function connect() {
    /* TODO: check if we're really "connected" */
    $("#span_username").text($.cookie("username"));
    $(".connected").show();
    $(".disconnected").hide();
    $(".dialog").hide();
    connected = true;
    retrieveLinks();
}

function disconnect() {
    $(".connected").hide();
    $(".disconnected").show();
    $(".dialog").hide();
    connected = false;
}

function retrieveLinks() {
    var result = $.ajax({
        type: "GET",
        url: "/users/" + $.cookie("username") + "/links",
        async: false,
    });
    if (result.status == 200)  {
        var xmlDoc = $.parseXML(result.responseText);
        var xml = $(xmlDoc);
        $("#links").text(""); /* clear the links list */
        xml.find("link").each(function() {
            var title = $(this).find("title").text();
            var url = $(this).find("url").text();
            var notes = $(this).find("notes").text();
            var tags = '<div class="tags">';
            $(this).find("tag").each(function() {
                tags += '<span class="tag">' + $(this).text() + '</span> - ';
            });
            tags = tags.substr(0, tags.length - 2) + '</div>';

            $("#links").append('<div class="link"><a href="' + url + 
                               '">' + title + '</a><br/><p>' + notes +
                               '</p><br/>' + tags + '<div>');
            $("#links").append('<br/>');
        });
    }
    else {
        message("Code " + result.status + ": " + result.responseText);
    }
}


$(document).ready(function() {
    /* Initialisation */
    $(".dialog").hide();
    $("#message").hide();
    $(".connected").hide();

    /* Close forms */
    $(".dialog").append('<a href="#" class="close">Close</a>');
    $(".close").click(function() {
        $('input[type="text"]').val("");
        $('input[type="password"]').val("");
        $('textarea').val("");
        $(this).parent().hide();
    });

    /* Connection and user creation */
    $("#connect").click(function() {
        $.cookie("username", $("#username").val());
        $.cookie("password", $("#password").val());
        connect();
    });
    $("#connect_popup").click(function() {
        $("#form_connect").show();
    });

    $("#disconnect").click(disconnect);

    $("#create_user_popup").click(function() {
        $("#form_create_user").show();
    });
    $("#create_user").click(function() {
        var user = new Clinks.User($("#create_username").val(),
                                   $("#create_password").val());
        message("Creating user...");
        user.onresponse = message_from_response;
        user.create(server_url);
    });

    /* Saving and updating links */
    $("#save_popup").click(function() {
        $("#form_save").show();
    });
    $("#save").click(function() {
        var link = new Clinks.Link($("#url").val(), $("#title").val(),
                                   $("#tags").val(), $("#notes").val(),
                                   $.cookie("username"), $.cookie("password"));
        message("Sending...");
        link.onresponse = message_from_response;
        link.create(server_url);
    });
    $("#update_links").click(retrieveLinks);
});