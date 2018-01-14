module.exports = {
  siteMetadata: {
    title: 'Deepak Jois — Website'
  },
  plugins: [
    {
      resolve: 'gatsby-source-filesystem',
      options: {
        path: `${__dirname}/src/content`,
        name: `content`
      }
    },
    {
      resolve: 'gatsby-transformer-remark'
    }
  ]
}
