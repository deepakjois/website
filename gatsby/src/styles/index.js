import { style } from 'glamor'
import { rhythm } from '../utils/typography'
import { colors } from './colors'

export default {
  colors,
  verticalPadding: style({
    padding: rhythm(3 / 4)
  }),
  container: style({
    maxWidth: `37rem`,
    margin: `0 auto`
  }),
  nav: style({
    height: rhythm(1.5),
    fontFamily: 'helvetica, sans-serif',
    fontVariant: 'small-caps',
    textAlign: 'center',
    '& ul': {
      display: 'inline-block',
      listStyle: 'none',
      margin: '0',
      '& li': {
        display: 'inline'
      },
      '& li + li:before': {
        content: '  ❧  '
      },
      '& a': {
        textDecoration: 'none'
      }
    }
  })
}
