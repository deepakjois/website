import React from 'react'

import styles from '../styles'
import { rhythm, scale } from '../utils/typography'
import presets from '../utils/presets'

import 'katex/dist/katex.min.css'

class BlogPostRoute extends React.Component {
  render() {
    const post = this.props.data.markdownRemark
    return (
      <div
        css={{
          maxWidth: rhythm(26)
        }}
      >
        <header>
          <h1>{post.frontmatter.title}</h1>
        </header>

        <div dangerouslySetInnerHTML={{ __html: post.html }} className="post" />
      </div>
    )
  }
}

export default BlogPostRoute

export const pageQuery = graphql`
  query BlogPostBySlug($slug: String!) {
    markdownRemark(fields: { slug: { eq: $slug } }) {
      html
      fields {
        tagSlugs
      }
      frontmatter {
        title
        tags
        date(formatString: "MMMM DD, YYYY")
      }
    }
  }
`
