import React from 'react'

const ArticleTemplate = ({data}) => {
    const article = data.markdownRemark
  return (
    <div>
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
