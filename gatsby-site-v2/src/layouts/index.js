import React from 'react'
import Link from 'gatsby-link'
import { rhythm, scale } from '../utils/typography'
import presets from '../utils/presets'

require(`prismjs/themes/prism-solarizedlight.css`)
require(`font-awesome/css/font-awesome.css`)

class DefaultLayout extends React.Component {
  render() {
    const { author, homepage } = this.props.data.site.siteMetadata
    return <div>{this.props.children()}</div>
  }
}

export default DefaultLayout

export const pageQuery = graphql`
  query LayoutIndexQuery {
    site {
      siteMetadata {
        author
        homepage
      }
    }
  }
`
