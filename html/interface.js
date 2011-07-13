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
    timeout = setTimeout(function () { $("#message").hide(); }, 5000);
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
    retrieve_links();
}

function disconnect() {
    $.cookie("username", null);
    $.cookie("password", null);
    $("#links").text(""); /* clear the links list */
    $(".connected").hide();
    $(".disconnected").show();
    $(".dialog").hide();
    connected = false;
}

function get_tags() {
    return $("#span_search_tags").text();
}

function delete_link(url) {
    var result = $.ajax({
        type: "DELETE",
        url: server_url + "/users/" + $.cookie("username") + "/links/" + url,
        async: false,
        beforeSend: function(req) {
            req.setRequestHeader("Authorization", "Basic " +
                                 btoa($.cookie("username") + ":" + 
                                      $.cookie("password")));
        }
    });
    if (result.status == 204)
        message("Deleted");
    else
        message_from_response(result.status, result.responseText);

    retrieve_links();
}

function edit_link(url) {
    var result = $.ajax({
        type: "GET",
        url: server_url + "/users/" + $.cookie("username") + "/links/" + url,
        async: false,
    });
    if (result.status == 200)  {
        var xmlDoc = $.parseXML(result.responseText);
        var xml = $(xmlDoc);
        var title = $.trim(xml.find("title").text());
        var url = $.trim(xml.find("url").text());
        var notes = $.trim(xml.find("notes").text());
        var tags = '';
        xml.find("tag").each(function() {
            tags += $.trim($(this).text()) + ',';
        });
        tags = tags.substr(0, tags.length - 1);

        $("#form_edit").show();
        $("#edit_title").val(title);
        $("#edit_url").val(url);
        $("#edit_notes").val(notes);
        $("#edit_tags").val(tags);
    }
    else {
        message_from_response(result.status, result.responseText);
    }

}

function retrieve_links() {
    var tags = get_tags();
    var url = server_url + "/users/" + $.cookie("username");
    if (tags == "")
        url += "/links";
    else
        url += "/tags/" + tags;

    var result = $.ajax({
        type: "GET",
        url: url,
        async: false,
    });
    if (result.status == 200)  {
        var xmlDoc = $.parseXML(result.responseText);
        var xml = $(xmlDoc);
        $("#links").text(""); /* clear the links list */
        xml.find("link").each(function() {
            var title = $.trim($(this).find("title").text());
            var url = $.trim($(this).find("url").text());
            var notes = $.trim($(this).find("notes").text());
            var tags = '<div class="tags">';
            $(this).find("tag").each(function() {
                tags += '<span class="tag">' + $.trim($(this).text()) + '</span> - ';
            });
            tags = tags.substr(0, tags.length - 2) + '</div>';

            $("#links").append('<div class="link"><a href="' + url + 
                               '">' + title + '</a>' +
                               '<div class="options">' + 
                               '<a href="#"' + 'onclick="edit_link(\'' + 
                               url + '\')">edit</a> ' +
                               '<a href="#"' + 'onclick="delete_link(\'' + 
                               url + '\')">delete</a>' +
                               '</div><p>' + notes +
                               '</p><br/>' + tags + '</div>');
            $("#links").append('<br/>');
        });
    }
    else {
        message_from_response(result.status, result.responseText);
    }
}

function add_search_tag(tag) {
    if ($("#span_search_tags").text() != "")
        $("#span_search_tags").append(",");

    $("#span_search_tags").append(tag);
    $("#search_tag").text("");
    retrieve_links();
}

function clear_search_tags() {
    $("#span_search_tags").text("");
    retrieve_links();
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
        $("#username").val("");
        $("#password").val("");
    });
    $("#connect_popup").click(function() {
        $("#form_connect").show();
    });

    $("#disconnect").click(disconnect);

    $("#create_user_popup").click(function() {
        $("#form_create_user").show();
    });
    $("#create_user").click(function() {
        /* create user */
        var user = new Clinks.User($("#create_username").val(),
                                   $("#create_password").val());
        message("Creating user and connecting...");
        user.onresponse = message_from_response;
        user.create(server_url);

        /* connect */
        $.cookie("username", $("#create_username").val());
        $.cookie("password", $("#create_password").val());
        connect();
        $("#form_save").hide();
    });

    /* Checking cookies */
    if ($.cookie("username")) {
        connect();
    }

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
        $("#form_save").hide();
        $("#title").val("");
        $("#url").val("");
        $("#tags").val("");
        $("#notes").val("");
        retrieve_links();
    });
    $("#edit").click(function () {
        var link = new Clinks.Link($("#edit_url").val(), $("#edit_title").val(),
                                   $("#edit_tags").val(), $("#edit_notes").val(),
                                   $.cookie("username"), $.cookie("password"));
        message("Sending...");
        link.onresponse = message_from_response;
        link.update(server_url);
        $("#form_edit").hide();
        $("#edit_title").val("");
        $("#edit_url").val("");
        $("#edit_tags").val("");
        $("#edit_notes").val("");
        retrieve_links();
    });
    $("#update_links").click(retrieve_links);

    /* Tag search */
    $("#search_tag").keypress(function (event) {
        if (event.which == 13) {
            $("#new_search_tag").trigger("click");
        }
    });
    $("#new_search_tag").click(function() {
        add_search_tag($("#search_tag").val());
        $("#search_tag").val("");
    });
    $("#clear_search_tags").click(clear_search_tags);
});