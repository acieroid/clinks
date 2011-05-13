from xml.dom.minidom import getDOMImplementation
import httplib, urllib

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

def request(method, url, input=None):
    headers = {'Content-type': 'application/x-www-form-urlencoded',
               'Accept': 'text/plain'}
    conn = httplib.HTTPConnection(server_url)
    conn.request(method, url, 'input=%s' % input, headers)
    print conn.getresponse().read(),

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
        request('POST', '/users/%s' % self.username, self.to_xml())
    def delete(self):
        request('DELETE', '/users/%s' % self.username, self.to_xml())

class Link:
    def __init__(self, url, title='', notes='', tags='', 
                 username=default_username, password=default_password):
        self.url = url
        self.title = title
        self.notes = notes
        self.tags = tags
        self.username = username
        self.password = password
    def to_xml(self):
        return to_xml('link', [(url, self.url),
                               (title, self.title),
                               (notes, self.notes),
                               (tags, self.tags)])
    def create(self):
        request('POST', '/users/%s/links/' % self.username, self.to_xml())
    def update(self):
        request('POST', '/users/%s/links/%s' % (self.username, self.url)
                self.to_xml())
    def delete(self):
        request('DELETE', '/users/%s/links/%s' % (self.username, self.url),
                self.to_xml())
