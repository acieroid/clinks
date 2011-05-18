from xml.dom.minidom import getDOMImplementation, parseString
from string import strip
from base64 import encodestring
import httplib, urllib
import argparse
import sys

server_url = 'localhost:8080'
default_username = 'foo'
default_password = 'bar'

impl = getDOMImplementation()

def to_dom(l, doc, node):
    for elem in l:
        child = doc.createElement(elem[0])
        child_text = doc.createTextNode(elem[1])
        child.appendChild(child_text)
        node.appendChild(child)

def to_xml(name, elements):
    doc = impl.createDocument(None, name, None)
    to_dom(elements, doc, doc.documentElement)
    res = doc.toxml()
    return res[res.find('<', 1):]

def checkError(status):
    if status == 401:
        raise ClinksError('User already exists')

def request(method, url, input=None, username=None, password=None):
    headers = {'Content-type': 'application/x-www-form-urlencoded',
               'Accept': 'text/plain'}
    if username and password:
        headers['Authorization'] = ('Basic ' + strip(encodestring(username + ':' + password)))
    conn = httplib.HTTPConnection(server_url)
    conn.request(method, url, 'input=%s' % input, headers)
    response = conn.getresponse()
    text = response.read()
    if (response.status != 200):
        print (text)
    return text

class User:
    def __init__(self, username, password):
        self.username = username
        self.password = password
    def to_xml(self):
        return to_xml('user', [('username', self.username), 
                               ('password', self.password)])
    def create(self):
        request('POST', '/users/', self.to_xml())
    def update(self):
        request('POST', '/users/%s' % self.username, self.to_xml(),
                self.username, self.password)
    def delete(self):
        request('DELETE', '/users/%s' % self.username, self.to_xml(),
                self.username, self.password)

class Link:
    def __init__(self, url, title='', tags='', notes='', 
                 username=default_username, password=default_password):
        self.url = url
        self.title = title
        self.notes = notes
        self.tags = tags
        self.username = username
        self.password = password
    def __str__(self):
        res = '"%s" (%s): %s' % (self.title, self.tags, self.url)
        if self.notes != '':
            res += '\n%s' % self.notes
        return res
    def to_xml(self):
        return to_xml('link', [('url', self.url),
                               ('title', self.title),
                               ('notes', self.notes),
                               ('tags', self.tags)])
    def create(self):
        request('POST', '/users/%s/links/' % self.username, self.to_xml(),
                self.username, self.password)
    def update(self):
        request('POST', '/users/%s/links/%s' % (self.username, self.url),
                self.to_xml(), self.username, self.password)
    def delete(self):
        request('DELETE', '/users/%s/links/%s' % (self.username, self.url),
                self.to_xml(), self.username, self.password)

def new_user(username, password):
    User(username, password).create()

def save(url, title, tags, notes):
    Link(url, title, tags, notes).create()

def getText(node, name, default=""):
    elements = node.getElementsByTagName(name)
    if elements != []:
        return elements[0].firstChild.toxml().rstrip().lstrip()
    else:
        return default

def retrieve(username):
    xml = request('GET', '/users/%s/links/' % username)
    dom = parseString(xml)
    for node in dom.firstChild.getElementsByTagName('link'):
        print (Link(getText(node, 'url'), getText(node, 'title'), 
                    getText(node, 'tags'), getText(node, 'notes'),
                    username))

def usage():
    print ('%s: possible actions are:' % sys.argv[0])
    print ('createuser username password')
    print ('\t create a new user')
    print ('save url [title [tags [notes]]]')
    print ('\t save a new url, tags are comma separated values')
    print ('retrieve [username]')
    print ('\t retrive the links of an user (default to your user)')

if __name__ == "__main__":
    length = len(sys.argv)
    if length == 1:
        usage()
    elif sys.argv[1] == 'createuser':
        if length != 4:
            usage()
        else:
            new_user(sys.argv[2], sys.argv[3])
    elif sys.argv[1] == 'save':
        save(sys.argv[2],
             (length >= 4 and sys.argv[3]) or "",
             (length >= 5 and sys.argv[4]) or "",
             (length >= 6 and sys.argv[5]) or "")
    elif sys.argv[1] == 'retrieve':
        retrieve((length == 3 and sys.argv[2]) or default_username)
