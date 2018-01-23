import React from 'react'
import Link from 'gatsby-link'
import { rhythm, scale } from '../utils/typography'
import styles from '../styles'
import presets from '../utils/presets'
import 'font-awesome/css/font-awesome.css'
import 'prismjs/themes/prism-solarizedlight.css'
import 'typeface-playfair-display'
import 'typeface-old-standard-tt'

class DefaultLayout extends React.Component {
  render() {
    const { author, homepage } = this.props.data.site.siteMetadata
    return (
      <div {...styles.container} {...styles.verticalPadding}>
        {this.props.children()}
      </div>
    )
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
