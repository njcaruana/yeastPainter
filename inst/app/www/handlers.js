$(document).ready(function() {
// Custom input handlers.
//
// Every one of these is a click event, so it is sent with priority "event".
// The default only notifies R when the value changes, which meant clicking the
// same entry twice in a row was silently dropped: the id sent was identical to
// the one before it. That is why a saved selection could be reopened for
// editing once, but not a second time.
function sendClick(inputId, el) {
  Shiny.setInputValue(inputId, el.id, {priority: 'event'});
}

$(document).on('click', '.example_link', function () {
  sendClick('example_link_id', this);
});
$(document).on('click', '.selectionRemove', function () {
  sendClick('selectionRemove_id', this);
});
$(document).on('click', '.labelRemove', function () {
  sendClick('labelRemove_id', this);
});
$(document).on('click', '.contactRemove', function () {
  sendClick('contactRemove_id', this);
});
$(document).on('click', '.selectionLink', function () {
  sendClick('selectionLink_id', this);
});
$(document).on('click', '.labelLink', function () {
  sendClick('labelLink_id', this);
});
$(document).on('click', '.contactLink', function () {
  sendClick('contactLink_id', this);
});
});
