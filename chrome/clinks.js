var Clinks = {
    Link : function(url, title, tags, notes, username, password) {
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
            /* the 'this' variable seems to point to the
             * XMLHttpRequest in the function below */
            onerror = this.onerror;
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
                    onerror(req.status, str);
                }
            }
            try {
                req.open("POST", server + "/users/" + this.user + "/links",
                         true);
                req.setRequestHeader("Content-Type",
                                     "application/x-www-form-urlencoded");
                req.setRequestHeader("Authorization", "Basic " +
                                     btoa(this.user + ":" + this.password));
                req.send("input=" + this.representation());
            }
            catch (error) {
                throw error;
                this.onerror(0, "Error when connecting to the server");
            }
        }
    }
}
