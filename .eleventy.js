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

  // Files to copy
  eleventyConfig.addPassthroughCopy("favicon.ico");
  eleventyConfig.addPassthroughCopy("s4c.css");
  eleventyConfig.addPassthroughCopy("w3.css");

  // Paths
  eleventyConfig.addPassthroughCopy("img");

  return eleventyConfig;
};
