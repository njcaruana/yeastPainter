// Composite snapshot: the NGL structure with the logFC legend drawn on top.
//
// Two things make this less obvious than it looks:
//
//  1. The NGL canvas cannot be captured with ctx.drawImage(). NGL builds its
//     WebGL context without preserveDrawingBuffer, so by the time a message
//     handler runs the backbuffer has already been cleared and the canvas
//     reads back blank. stage.makeImage() re-renders into an offscreen target
//     and resolves with a blob, which is the supported way to capture it.
//
//  2. The legend is painted here with the Canvas 2D API rather than being
//     rasterised from the DOM, so the app needs no external JS library.
//     Keep the colours and metrics below in sync with .logfc-legend,
//     .logfc-ticks and .logfc-gradient in styles.css.

function roundRectPath(ctx, x, y, w, h, r) {
  r = Math.min(r, w / 2, h / 2);
  ctx.beginPath();
  ctx.moveTo(x + r, y);
  ctx.lineTo(x + w - r, y);
  ctx.quadraticCurveTo(x + w, y, x + w, y + r);
  ctx.lineTo(x + w, y + h - r);
  ctx.quadraticCurveTo(x + w, y + h, x + w - r, y + h);
  ctx.lineTo(x + r, y + h);
  ctx.quadraticCurveTo(x, y + h, x, y + h - r);
  ctx.lineTo(x, y + r);
  ctx.quadraticCurveTo(x, y, x + r, y);
  ctx.closePath();
}

// Height of the strip added beneath the structure to hold the legend.
function legendStripHeight(width) {
  var scale = Math.max(1, width / 900);
  return Math.round((10 * 2 + 13 + 6 + 20 + 16) * scale);
}

// Draws the legend into a band BELOW the structure rather than on top of it.
// Overlaying meant the gradient sat across the molecule whichever way the
// user had rotated the view; a dedicated strip can never collide with it.
//
// `opaquePanel` paints a white backing behind the legend. It is skipped for
// transparent captures, where a white block would show up as a rectangle on
// whatever the image is placed against.
function drawLogfcLegend(ctx, canvas, legend, stripTop, opaquePanel) {
  var scale = Math.max(1, canvas.width / 900);

  var padX = 12 * scale,
      padY = 10 * scale,
      barH = 20 * scale,
      font = 13 * scale,
      gap  = 6 * scale,
      boxW = 200 * scale;

  var boxH = padY * 2 + font + gap + barH;
  var x = (canvas.width - boxW) / 2;
  var y = stripTop + (legendStripHeight(canvas.width) - boxH) / 2;

  ctx.save();

  if (opaquePanel) {
    ctx.fillStyle = 'rgba(255, 255, 255, 0.9)';
    roundRectPath(ctx, x, y, boxW, boxH, 6 * scale);
    ctx.fill();
  }

  ctx.fillStyle = opaquePanel ? '#222222' : '#111111';
  ctx.font = '500 ' + font + 'px sans-serif';
  ctx.textBaseline = 'top';

  var textY = y + padY;
  ctx.textAlign = 'left';
  ctx.fillText(legend.min, x + padX, textY);
  ctx.textAlign = 'center';
  ctx.fillText(legend.mid, x + boxW / 2, textY);
  ctx.textAlign = 'right';
  ctx.fillText(legend.max, x + boxW - padX, textY);

  var barX = x + padX,
      barY = textY + font + gap,
      barW = boxW - padX * 2;

  var grad = ctx.createLinearGradient(barX, 0, barX + barW, 0);
  grad.addColorStop(0,   '#2166ac');
  grad.addColorStop(0.5, '#f7f7f7');
  grad.addColorStop(1,   '#b2182b');

  roundRectPath(ctx, barX, barY, barW, barH, 4 * scale);
  ctx.fillStyle = grad;
  ctx.fill();
  ctx.strokeStyle = '#999999';
  ctx.lineWidth = Math.max(1, scale);
  ctx.stroke();

  ctx.restore();
}

function snapshotFailed(message, reason) {
  console.error('composite-snapshot: ' + reason);
  if (window.Shiny && message && message.statusInput) {
    Shiny.setInputValue(message.statusInput, reason, { priority: 'event' });
  }
}

// Hand the rendered canvas back to R as a data URL instead of downloading it.
// Used by the report tab, which needs the structure as a still image.
function returnCanvasToShiny(canvas, message) {
  try {
    Shiny.setInputValue(message.returnTo, canvas.toDataURL('image/png'),
                        { priority: 'event' });
    if (message.statusInput) {
      Shiny.setInputValue(message.statusInput, 'ok', { priority: 'event' });
    }
  } catch (err) {
    snapshotFailed(message, 'canvas-read-failed: ' + err);
  }
}

function downloadCanvas(canvas, fileName, message) {
  canvas.toBlob(function (out) {
    if (!out) {
      snapshotFailed(message, 'encode-failed');
      return;
    }
    var url = URL.createObjectURL(out);
    var link = document.createElement('a');
    link.download = fileName + '.png';
    link.href = url;
    // Firefox needs the anchor in the document for a synthetic click to work.
    document.body.appendChild(link);
    link.click();
    document.body.removeChild(link);
    setTimeout(function () { URL.revokeObjectURL(url); }, 1000);

    if (window.Shiny && message.statusInput) {
      Shiny.setInputValue(message.statusInput, 'ok', { priority: 'event' });
    }
  }, 'image/png');
}

Shiny.addCustomMessageHandler('composite-snapshot', function (message) {
  if (typeof getNGLStage !== 'function') {
    snapshotFailed(message, 'ngl-unavailable');
    return;
  }

  var stage = getNGLStage(message.nglId);
  if (!stage) {
    snapshotFailed(message, 'no-structure');
    return;
  }

  stage.makeImage({
    factor:      message.factor || 1,
    antialias:   !!message.antialias,
    trim:        !!message.trim,
    transparent: !!message.transparent
  }).then(function (blob) {
    var url = URL.createObjectURL(blob);
    var img = new Image();

    img.onload = function () {
      URL.revokeObjectURL(url);

      // Grow the canvas so the legend gets its own band under the structure
      // instead of being painted over it.
      var strip = message.legend ? legendStripHeight(img.width) : 0;

      var canvas = document.createElement('canvas');
      canvas.width  = img.width;
      canvas.height = img.height + strip;

      var ctx = canvas.getContext('2d');

      // A transparent capture leaves both the structure background and the
      // legend strip clear; otherwise fill so the strip is not see-through.
      if (!message.transparent) {
        ctx.fillStyle = message.stripColor || '#FFFFFF';
        ctx.fillRect(0, 0, canvas.width, canvas.height);
      }

      ctx.drawImage(img, 0, 0);

      if (message.legend) {
        drawLogfcLegend(ctx, canvas, message.legend, img.height,
                        !message.transparent);
      }

      if (message.returnTo) {
        returnCanvasToShiny(canvas, message);
      } else {
        downloadCanvas(canvas, message.fileName, message);
      }
    };

    img.onerror = function () {
      URL.revokeObjectURL(url);
      snapshotFailed(message, 'image-decode-failed');
    };

    img.src = url;
  }).catch(function (err) {
    snapshotFailed(message, 'makeImage-failed: ' + err);
  });
});
