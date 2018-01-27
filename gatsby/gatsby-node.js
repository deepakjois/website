const _ = require(`lodash`)
const Promise = require(`bluebird`)
const path = require(`path`)
const slash = require(`slash`)
const { createFilePath } = require(`gatsby-source-filesystem`)

const webpackLodashPlugin = require(`lodash-webpack-plugin`)
const StaticSiteGeneratorWebpackPlugin = require(`static-site-generator-webpack-plugin`)
const AlternateStaticSiteGeneratorWebpackPlugin = require(`./src/webpack/alternate-static-site-generator-webpack-plugin`)

exports.createPages = ({ graphql, boundActionCreators }) => {
  const { createPage } = boundActionCreators

  return new Promise(resolve => {
    const postTemplate = path.resolve(`src/templates/template-post.js`)
    graphql(
      `
        {
          allMarkdownRemark(
            limit: 1000
            filter: { frontmatter: { draft: { ne: true } } }
          ) {
            edges {
              node {
                fields {
                  slug
                }
                frontmatter {
                  tags
                }
              }
            }
          }
        }
      `
    ).then(result => {
      if (result.errors) {
        console.log(result.errors)
      }

      // Create blog posts pages.
      result.data.allMarkdownRemark.edges.forEach(edge => {
        createPage({
          path: edge.node.fields.slug, // Required
          component: slash(postTemplate),
          context: {
            slug: edge.node.fields.slug,
            highlight: edge.node.frontmatter.highlight,
            shadow: edge.node.frontmatter.shadow
          }
        })
      })

      resolve()
    })
  })
}

// Add custom url pathname for blog posts.
exports.onCreateNode = ({ node, boundActionCreators, getNode }) => {
  const { createNodeField } = boundActionCreators

  if (node.internal.type === `MarkdownRemark`) {
    const value = createFilePath({ node, getNode, trailingSlash: false })
    createNodeField({
      name: `slug`,
      node,
      value
    })
    if (node.frontmatter.tags) {
      const tagSlugs = node.frontmatter.tags.map(
        tag => `/tags/${_.kebabCase(tag)}/`
      )
      createNodeField({ node, name: `tagSlugs`, value: tagSlugs })
    }
  }
}

// Sass and Lodash.
exports.modifyWebpackConfig = ({ config, stage }) => {
  let idx, plugin
  switch (stage) {
    case 'build-javascript':
      config.plugin(`Lodash`, webpackLodashPlugin, null)
      break
    case 'build-html':
      // Modify HTML building to generate files w/ no extension
      // instead of generating a folder witn a single index.html
      // file in it
      idx = _.findIndex(
        config._config.plugins,
        o => o instanceof StaticSiteGeneratorWebpackPlugin
      )
      plugin = config._config.plugins[idx]

      // Replace StaticSiteGeneratorWebpackPlugin w/ our custom version
      config._config.plugins[
        idx
      ] = new AlternateStaticSiteGeneratorWebpackPlugin({
        entry: plugin.entry,
        paths: plugin.paths
      })
      break
  }

  return config
}
