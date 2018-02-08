import React from 'react'
import { Helmet } from 'react-helmet'
import { rhythm } from '../utils/typography'
import Nav from '../components/Nav'

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
        <Helmet>
          <title>{post.frontmatter.title}</title>
        </Helmet>
        <Nav />
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
