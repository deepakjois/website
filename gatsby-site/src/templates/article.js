import React from 'react'
import Helmet from 'react-helmet'

const ArticleTemplate = (props) => {
    const article = props.data.markdownRemark
  return (
    <div>
    <Helmet
      title={article.frontmatter.title}
    />
      <h1>{article.frontmatter.title}</h1>
      <div dangerouslySetInnerHTML={{ __html: article.html }} />
    </div>
    )
}

export default ArticleTemplate
export const articleQuery = graphql`
  query ArticleSlug($slug: String!) {
    markdownRemark(fields: { slug: { eq: $slug } }) {
      id
      html
      frontmatter {
        title
      }
    }
  }
`
