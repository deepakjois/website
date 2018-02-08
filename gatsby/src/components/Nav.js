import React from 'react'
import styles from '../styles'

class Nav extends React.Component {
  render() {
    return (
      <nav {...styles.nav}>
        <ul>
          <li>
            <a href="/">home</a>
          </li>
          <li>
            <a href="/posts">all</a>
          </li>
        </ul>
      </nav>
    )
  }
}

export default Nav
