const CleanCSS = require("clean-css");

module.exports = function(eleventyConfig) {
  // Customized Markdown Export.
  let markdownIt = require("markdown-it");
  let md = markdownIt({html: true})
  md.use(require("markdown-it-deflist"));
  md.use(require("markdown-it-anchor"));
  eleventyConfig.setLibrary("md", md);
  eleventyConfig.addPairedShortcode("markdown", (content) => {
    return md.render(content);
  });

  // clean-css
  eleventyConfig.addFilter("cssmin", function(code) {
    return new CleanCSS({}).minify(code).styles;
  });

  return eleventyConfig;
};
