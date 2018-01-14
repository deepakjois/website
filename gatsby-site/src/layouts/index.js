import React from 'react'
import PropTypes from 'prop-types'
import Helmet from 'react-helmet'

// FIXME remove wrapper div when react fragment support lands
const TemplateWrapper = ({ data, children }) => {
  return (<div>
    <Helmet
      title={data.site.siteMetadata.title}
      meta={[ { name: 'description', content: 'My website' } ]}
    />
    <h1>This is in the layout</h1>
    {children()}
</div>)
}

TemplateWrapper.propTypes = {
  children: PropTypes.func
}

export default TemplateWrapper

export const pageQuery = graphql`
  query IndexQuery {
    site {
      siteMetadata {
        title
      }
    }
  }
`
