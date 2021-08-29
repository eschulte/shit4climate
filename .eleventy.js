const csv = require("csv-parse")
var Readable = require('stream').Readable
const yaml = require("js-yaml");
const CleanCSS = require("clean-css");

module.exports = function(eleventyConfig) {
  // Customized Markdown Export.
  let markdownIt = require("markdown-it");
  let md = markdownIt({html: true})
  md.use(require("markdown-it-deflist"));
  md.use(require("markdown-it-anchor"));
  eleventyConfig.setLibrary("md", md);

  // clean-css
  eleventyConfig.addFilter("cssmin", function(code) {
    return new CleanCSS({}).minify(code).styles;
  });

  eleventyConfig.addPairedShortcode("markdown", (content) => {
    return md.render(content);
  });

  // Parse CSV
  eleventyConfig.addDataExtension("csv", (contents) => {
    var s = new Readable()
    s.push(contents)
    s.push(null)
    results = [];
    s.pipe(csv()).on('data', (data) => results.push(data));
    return results;
  });

  // Parse YAML
  eleventyConfig.addDataExtension("yaml", contents => yaml.load(contents));

  // Files to copy
  // eleventyConfig.addPassthroughCopy("favicon.ico");

  // Paths
  // eleventyConfig.addPassthroughCopy("img");

  return eleventyConfig;
};
