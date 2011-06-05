var Clinks = {
    Link : function(url, title, tags, notes, username, password) {
        this.url = url;
        this.title = title;
        this.tags = tags;
        this.notes = notes;
        this.username = username;
        this.password = password;

        this.response = function(status, str) {
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
            onresponse = this.onresponse;
            req = new XMLHttpRequest();
            req.onreadystatechange = function() {
                if (req.readyState == 4) {
                    onresponse(req.status, req.responseText);
                }
            }
            try {
                req.open("POST", server + "/users/" + this.username + "/links",
                         true);
                req.setRequestHeader("Content-Type",
                                     "application/x-www-form-urlencoded");
                req.setRequestHeader("Authorization", "Basic " +
                                     btoa(this.username + ":" + this.password));
                req.send("input=" + this.representation());
            }
            catch (error) {
                this.onresponse(0, "Error when connecting to the server");
            }
        }
    }
}
