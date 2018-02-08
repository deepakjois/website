module.exports = {
  siteMetadata: {
    title: `gatsby-example-using-remark`,
    author: `@gatsbyjs`,
    description: `Blazing-fast React.js static site generator`,
    homepage: `https://www.gatsbyjs.org`
  },
  plugins: [
    {
      resolve: `gatsby-plugin-typography`,
      options: {
        pathToConfigModule: `src/utils/typography.js`
      }
    },
    {
      resolve: `gatsby-source-filesystem`,
      options: {
        path: `${__dirname}/src/pages`,
        name: `pages`
      }
    },
    {
      resolve: `gatsby-transformer-remark`,
      options: {
        excerpt_separator: `<!-- end -->`,
        plugins: [
          {
            resolve: `gatsby-remark-responsive-iframe`,
            options: {
              wrapperStyle: `margin-bottom: 1.0725rem`
            }
          },
          {
            resolve: `gatsby-remark-smartypants`,
            options: {
              dashes: `oldschool`
            }
          },
          `gatsby-remark-prismjs`,
          `gatsby-remark-autolink-headers`,
          `gatsby-remark-katex`
        ]
      }
    },
    `gatsby-plugin-catch-links`,
    `gatsby-plugin-glamor`,
    `gatsby-plugin-remove-trailing-slashes`,
    `gatsby-plugin-react-helmet`
  ]
}
