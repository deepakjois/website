import React from 'react'
import styles from '../styles'
import 'font-awesome/css/font-awesome.css'
import 'prismjs/themes/prism-solarizedlight.css'
import 'typeface-playfair-display'
import 'typeface-old-standard-tt'
import './layout.css'

class DefaultLayout extends React.Component {
  render() {
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
