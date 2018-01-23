import { style } from 'glamor'
import { rhythm } from '../utils/typography'
import { colors } from './colors'
import presets from '../utils/presets'

export default {
  colors,
  verticalPadding: style({
    padding: rhythm(3 / 4)
  }),
  container: style({
    maxWidth: `37rem`,
    margin: `0 auto`
  })
}
