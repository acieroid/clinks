var Clinks = {
    request : function(onresponse, method, page, input, username, password) {
        req = new XMLHttpRequest();
        req.onreadystatechange = function() {
            if (req.readyState == 4) {
                if (req.status == 0)
                    onresponse(0, "Error when connecting to the server");
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
                                     btoa(this.username + ":" + this.password));
            }
            req.send("input=" + this.representation());
        }
        catch (error) {
            onresponse(0, "Error when connecting to the server");
        }
    }

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
            request(this.onresponse, "POST", server + "/users/" + 
                    this.username + "/links", this.representation());
        }


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
            request(this.onresponse, "POST", server + "/users/" +
                    this.username + "/links", this.representation(),
                    this.username, this.password);
        }
    }
}
