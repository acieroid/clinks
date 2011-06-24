/* TODO: use cookies */
var username = "";
var password = "";
var connected = false;
var timeout = false;

function message(str) {
    $("#message").show();
    $("#message").text(str);
    if (timeout) {
        clearTimeout(timeout);
        timeout = false;
    }
    timeout = setTimeout(function () { $("#message").hide(); }, 3000);
}

function connect() {
    /* TODO: check if we're really "connected" */
    $(".connected").append(" - Connected as " + username);
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
        url: "/users/" + username + "/links",
        async: false,
    });
    if (result.status == 200)  {
        var xmlDoc = $.parseXML(result.responseText);
        var xml = $(xmlDoc);
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

    /* Connection */
    $("#connect").click(function() {
        username = $('#username').val();
        password = $('#password').val();
        connect();
    });
    $("#connect_popup").click(function() {
        $("#form_connect").show();
    });
    $("#disconnect").click(disconnect);

    /* Saving */
    $("#save_popup").click(function() {
        $("#form_save").show();
    });
    $("#save").click(function() {
        link = new Clinks.Link($("#url").val(), $("#title").val(),
                               $("#tags").val(), $("#notes").val(),
                               username, password);
        message("Sending...");
        link.onresponse = function(status, str) {
            message(str);
        }
        link.create("");
    });
});