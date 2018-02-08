import React from 'react'
import logodeva from './logo-deva.svg'
import logolatn from './logo-latn.svg'
import { Helmet } from 'react-helmet'

export default function Index() {
  const choice = Math.round(Math.random())
  const logo = [logodeva, logolatn][choice]
  return (
    <div className="container">
      <Helmet>
        <title>Deepak Jois – Website</title>
      </Helmet>
      <img
        css={{ height: '10em', display: 'block', margin: '0 auto' }}
        className="logo"
        src={logo}
        alt="logo"
      />

      <h1 css={{ textAlign: 'center' }}>Deepak Jois</h1>

      <hr />
      <p>
        Programmer and{' '}
        <a href="https://en.wikipedia.org/wiki/Free_software">
          free (libre) software
        </a>{' '}
        enthusiast from India. Previously used to work at{' '}
        <a href="https://soundcloud.com">Soundcloud</a> and{' '}
        <a href="https://aws.amazon.com">Amazon Web Services</a>
        , among other places.
      </p>
      <ul className="links">
        <li>
          <i className="fa fa-envelope" aria-hidden="true" />{' '}
          <a href="mailto:deepak.jois@gmail.com">Email</a>
        </li>

        <li>
          <i className="fa fa-github" aria-hidden="true" />{' '}
          <a href="https://github.com/deepakjois">Github</a>
        </li>

        <li>
          <i className="fa fa-twitter" aria-hidden="true" />{' '}
          <a href="http://twitter.com/debugjois">Twitter</a>
        </li>
        <li>
          <i className="fa fa-medium" aria-hidden="true" />{' '}
          <a href="https://medium.com/@debugjois">Medium</a>
        </li>
        <li>
          <i className="fa fa-bandcamp" aria-hidden="true" />{' '}
          <a href="https://bandcamp.com/deepakjois/wishlist">Bandcamp</a>
        </li>
      </ul>
    </div>
  )
}
