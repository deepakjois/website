import React from 'react'
import logodeva from './logo-deva.svg'
import logolatn from './logo-latn.svg'

export default () => {
  const choice = Math.round(Math.random())
  const logo = [logodeva, logolatn][choice]
  return (
    <div className="container">
      <img className="logo" src={logo} alt="logo" />

      <h1>Deepak Jois</h1>

      <h4>
        Programmer and{' '}
        <a href="https://en.wikipedia.org/wiki/Free_software">
          free (libre) software
        </a>{' '}
        enthusiast from India. Previously used to work at{' '}
        <a href="https://soundcloud.com">Soundcloud</a> and{' '}
        <a href="https://aws.amazon.com">Amazon Web Services</a>
        , among other places.
      </h4>

      <ul className="links">
        <li>
          <h3>
            <i className="fa fa-envelope" aria-hidden="true" />{' '}
            <a href="mailto:deepak.jois@gmail.com">Email</a>
          </h3>
        </li>

        <li>
          <h3>
            <i className="fa fa-github" aria-hidden="true" />{' '}
            <a href="https://github.com/deepakjois">Github</a>
          </h3>
        </li>

        <li>
          <h3>
            <i className="fa fa-twitter" aria-hidden="true" />{' '}
            <a href="http://twitter.com/debugjois">Twitter</a>
          </h3>
        </li>
        <li>
          <h3>
            <i className="fa fa-medium" aria-hidden="true" />{' '}
            <a href="https://medium.com/@debugjois">Medium</a>
          </h3>
        </li>
        <li>
          <h3>
            <i className="fa fa-bandcamp" aria-hidden="true" />{' '}
            <a href="https://bandcamp.com/deepakjois/wishlist">Bandcamp</a>
          </h3>
        </li>
      </ul>
    </div>
  )
}
