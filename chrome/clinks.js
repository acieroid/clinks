var Clinks {
    function Link(url, title, tags, notes, username, password) {
        this.url = url;
        this.title = title;
        this.tags = tags;
        this.notes = notes;
        this.username = username;
        this.password = password;

        this.onerror = function(status, str) {
            alert(str);
        }

        this.representation = function() {
            return ("<link>" +
                    "<url>" + this.url + "</url>" +
                    "<title>" + this.title + "</title>" +
                    "<notes>" + this.notes + "</notes>" +
                    "<tags>" + this.tags + "</tags>" +
                    "</link>");
        }

        this.create = function(server) {
            req = new XMLHttpRequest();
            req.onreadystatechange = function() {
                if (req.readyState == 4) {
                    var str;
                    switch (req.status) {
                    case 0:
                        str = "Error when submitting, check your configuration";
                        break;
                    case 401:
                         str = "Permission denied, check your identification settings";
                        break;
                    case 415:
                        str = "Representation not parsable";
                        break;
                    case 201:
                        str = "Link saved !";
                        break;
                    default:
                        str = "Unknown return code";
                    }
                    this.onerror(req.status, str);
                }
            }
            try {
                /* TODO: HTTP basic auth */
                req.open("POST", server + "/users/" + this.user + "/links",
                         true);
                req.setRequestHeader("Content-Type",
                                     "application/x-www-form-urlencoded");
                req.send("input=" + this.representation());
            }
            catch {
                this.onerror(0, "Error when connecting to the server");
            }
        }
    }
}
