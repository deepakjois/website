import React from 'react'
import PropTypes from 'prop-types'
import Helmet from 'react-helmet'

const TemplateWrapper = ({data, children, location}) => {
  if (location.path == "/") {
    return IndexTemplate(data, children)
  }
    return ArticleTemplate(children)
}

TemplateWrapper.propTypes = {
  children: PropTypes.func
}

// FIXME remove wrapper div when react fragment support lands
const IndexTemplate = (data, children) => (<div>
    <Helmet
      title={data.site.siteMetadata.title}
      meta={[ { name: 'description', content: 'Deepak’s Website' } ]}
    />
    {children()}
  </div>)

// FIXME remove wrapper div when react fragment support lands
const ArticleTemplate = (children) => <div>{children()}</div>


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
