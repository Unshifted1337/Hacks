module.exports = rootrelative;
var
  p         = require('path'),
  fs        = require('fs'),
  mainpath  = p.dirname((function () {
    try { return module.parent.filename; }
    catch (e) { return process.argv[1]; }
  })()),
  rootpath  = (function findroot (path) {
    if (fs.existsSync(p.join(path, 'package.json'))) { return path; }
    path = path.split(p.sep);
    path.pop();
    path = path.join(p.sep);
    if (!path.length) { throw new Error('package.json not found in path') }
    return findroot(path);
  })(mainpath)
;
function rootrelative (path) {
  return p.join(rootpath,path);
};
