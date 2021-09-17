const CleanCSS = require("clean-css");
const UglifyJS = require("uglify-js");

function sortByOrder(values) {
    let vals = [...values]; // this *seems* to prevent collection mutation...
    return vals.sort((a, b) => Math.sign(a.data.order - b.data.order));
}

module.exports = function(eleventyConfig) {
  // Customized Markdown Export.
  let markdownIt = require("markdown-it");
  let md = markdownIt({html: true})
  md.use(require("markdown-it-deflist"));
  md.use(require("markdown-it-anchor"));
  md.use(require("markdown-it-footnote"));
  eleventyConfig.setLibrary("md", md);
  eleventyConfig.addPairedShortcode("markdown", (content) => {
    return md.render(content);
  });

  // clean-css
  eleventyConfig.addFilter("cssmin", function(code) {
    return new CleanCSS({}).minify(code).styles;
  });

  // Uglify-JS
  eleventyConfig.addFilter("jsmin", function(code) {
    let minified = UglifyJS.minify(code);
    if( minified.error ) {
      console.log("UglifyJS error: ", minified.error);
      return code;
    }

    return minified.code;
  });

  // Sort collections by an "order" field
  eleventyConfig.addFilter("sortByOrder", sortByOrder);

  // Copy through the icons
  eleventyConfig.addPassthroughCopy("favicon.ico");
  eleventyConfig.addPassthroughCopy("apple-touch-icon.png");
  eleventyConfig.addPassthroughCopy("javascript.js");

  return eleventyConfig;
};
