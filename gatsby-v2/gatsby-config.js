module.exports = {
  siteMetadata: {
    title: 'Deepak Jois – Website',
  },
  plugins: [
    'gatsby-plugin-react-helmet',
    `gatsby-plugin-glamor`,
    {
      resolve: `gatsby-plugin-typography`,
      options: {
        pathToConfigModule: `src/utils/typography`,
      },
    },
  ],
}
