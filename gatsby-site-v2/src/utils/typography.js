import Typography from 'typography'

const typography = new Typography({
  /*
  googleFonts: [
  {
    name: 'Playfair Display',
    styles: [
      '400',
      '400i',
      '700',
      '700i'
    ],
  },
  {
    name: 'Old Standard TT',
    styles: [
      '400',
      '400i',
      '700'
    ],
  },
  ],
  */
  baseFontSize: '20px',
  bodyGray: 0,
  headerFontFamily: ['Playfair Display', 'sans-serif'],
  bodyFontFamily: ['Old Standard TT', 'serif']
})

module.exports = typography
