var RawSource = require('webpack-sources/lib/RawSource')
var evaluate = require('eval')
var Promise = require('bluebird')

// An alternative static site generator customized for gatsby,
// and other tweaks to generate HTML files w/o extensions.
function AlternateStaticSiteGeneratorWebpackPlugin(options) {
  options = options || {}

  this.entry = options.entry
  this.paths = options.paths
}

AlternateStaticSiteGeneratorWebpackPlugin.prototype.apply = function(compiler) {
  var self = this

  compiler.plugin('this-compilation', function(compilation) {
    compilation.plugin('optimize-assets', function(_, done) {
      var webpackStats = compilation.getStats()

      try {
        var asset = compilation.assets[self.entry]

        if (asset == null) {
          throw new Error('Source file not found: "' + self.entry + '"')
        }

        var source = asset.source()
        var render = evaluate(
          source,
          /* filename: */ self.entry,
          /* scope: */ self.globals,
          /* includeGlobals: */ true
        )

        if (typeof render !== 'function') {
          throw new Error(
            'Export from "' +
              self.entry +
              '" must be a function that returns an HTML string. Is output.libraryTarget in the configuration set to "umd"?'
          )
        }

        renderPaths(self.paths, render, webpackStats, compilation).nodeify(done)
      } catch (err) {
        compilation.errors.push(err.stack)
        done()
      }
    })
  })
}

function renderPaths(paths, render, webpackStats, compilation) {
  var renderPromises = paths.map(function(outputPath) {
    var locals = {
      path: outputPath,
      webpackStats: webpackStats
    }

    var renderPromise = Promise.fromNode(render.bind(null, locals))

    return renderPromise
      .then(function(output) {
        var assetName = pathToAssetName(outputPath)
        compilation.assets[assetName] = new RawSource(output)
      })
      .catch(function(err) {
        compilation.errors.push(err.stack)
      })
  })

  return Promise.all(renderPromises)
}

function pathToAssetName(outputPath) {
  if (outputPath == '/') {
    return 'index.html'
  }
  return outputPath
}

module.exports = AlternateStaticSiteGeneratorWebpackPlugin
