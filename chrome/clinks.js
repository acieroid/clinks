var Clinks = {
    request : function(onresponse, method, page, input, username, password) {
        req = new XMLHttpRequest();
        req.onreadystatechange = function() {
            if (req.readyState == 4) {
                if (req.status == 0)
                    onresponse(0, "<error>Error when connecting to the server</error>");
                else
                    onresponse(req.status, req.responseText);
            }
        }
        try {
            req.open(method, page, true);
            req.setRequestHeader("Content-Type",
                                 "application/x-www-form-urlencoded");
            if (username != undefined && password != undefined) {
                req.setRequestHeader("Authorization", "Basic " +
                                     btoa(username + ":" + password));
            }
            req.send("input=" + input);
        }
        catch (error) {
            onresponse(0, "<error>Error when connecting to the server</error>");
        }
    },

    User : function(username, password) {
        this.username = username;
        this.password = password;

        this.response = function(status, str) {
            alert(str);
        }

        this.representation = function() {
            return ("<user>" + 
                    "<username>" + this.username + "</username>" +
                    "<password>" + this.password + "</password>" +
                    "</user>");
        }

        this.create = function(server) {
            Clinks.request(this.onresponse, "POST", server + "/users/",
                           this.representation());
        }
    },

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
            Clinks.request(this.onresponse, "POST", server + "/users/" +
                    this.username + "/links", this.representation(),
                    this.username, this.password);
        }
    }
}
