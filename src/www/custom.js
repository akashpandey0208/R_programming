// =============================================================================
// ADMIN TABLE — DataTables helpers
// =============================================================================

function getDT(ns){
  var wrap = $('#' + ns + 'table');
  if(wrap.length === 0) return null;
  var tbl = wrap.find('table');
  if(tbl.length === 0) return null;
  if($.fn.dataTable.isDataTable(tbl)) return tbl.DataTable();
  return null;
}

function disableNativeHeaderSort(ns){
  var out = $('#' + ns + 'table');
  if(out.length === 0) return;
  var dtWrap = out.closest('.dataTables_wrapper');
  if(dtWrap.length === 0) return;
  var head = dtWrap.find('.dataTables_scrollHead thead');
  if(head.length === 0) head = dtWrap.find('thead');
  head.find('th')
    .off('click.DT').off('mousedown.DT').off('mouseup.DT')
    .off('pointerdown.DT').off('touchstart.DT');
  head.find('.th-title').off('click').on('click', function(e){
    e.preventDefault(); e.stopPropagation(); e.stopImmediatePropagation(); return false;
  });
  head.find('.filter-ico').off('click.DT');
}

// Export dropdown
$(document).on('click', "[id$='exportBtn']", function(e){
  e.preventDefault(); e.stopPropagation();
  var ns = $(this).attr('id').replace('exportBtn','');
  $("[id$='exportMenu']").addClass('hidden');
  $('#' + ns + 'exportMenu').toggleClass('hidden');
});
$(document).on('click', "[id$='exportMenu']", function(e){ e.stopPropagation(); });
$(document).on('click', function(){ $("[id$='exportMenu']").addClass('hidden'); });

function clickDownload(ns, dlId){ var btn = $('#' + ns + dlId); if(btn.length) btn[0].click(); }

$(document).on('click', "[id$='export_csv']", function(e){
  e.preventDefault(); e.stopPropagation();
  var ns = $(this).attr('id').replace('export_csv','');
  $('#' + ns + 'exportMenu').addClass('hidden');
  clickDownload(ns, 'dl_csv');
});
$(document).on('click', "[id$='export_xlsx']", function(e){
  e.preventDefault(); e.stopPropagation();
  var ns = $(this).attr('id').replace('export_xlsx','');
  $('#' + ns + 'exportMenu').addClass('hidden');
  clickDownload(ns, 'dl_xlsx');
});

// Global search
$(document).on('input', '.global-search', function(){
  var ns = $(this).attr('id').replace('global_search','');
  var dt = getDT(ns);
  if(!dt) return;
  dt.search(this.value).draw(false);
  setTimeout(function(){ updateFilterIconState(ns); updateSortIconState(ns); renderCustomPager(ns); }, 20);
});

// Clear all filters
Shiny.addCustomMessageHandler('dt_clear_all', function(msg){
  var ns = msg.ns;
  var dt = getDT(ns);
  if(!dt) return;
  dt.search('').columns().search('').order([]).page('first').draw(false);
  $('#' + ns + 'global_search').val('');
  setTimeout(function(){ updateFilterIconState(ns); updateSortIconState(ns); renderCustomPager(ns); }, 50);
});

// Column selector init
Shiny.addCustomMessageHandler('initColumns', function(msg){
  function tryInit(attemptsLeft) {
    var ns = msg.ns;
    var $list = $('#' + ns + 'colList');
    if ($list.length === 0) {
      if (attemptsLeft > 0) {
        setTimeout(function(){ tryInit(attemptsLeft - 1); }, 100);
      }
      return;
    }


    var table = $('#' + ns + 'table table').DataTable();

    $list.empty();

    (msg.names || []).forEach(function(name, i){

      var isVisible = table.column(i).visible();

      $list.append(
        '<label class="col-item">' +
          '<input type="checkbox" data-idx="' + i + '" ' + (isVisible ? 'checked' : '') + '>' +
          '<span>' + name + '</span>' +
        '</label>'
      );
    });


    // 3) Then run your other UI initializers
    setTimeout(function(){
      if (typeof decorateHeaders === 'function')       decorateHeaders(ns);
      if (typeof disableNativeHeaderSort === 'function') disableNativeHeaderSort(ns);
      if (typeof updateColCounter === 'function')      updateColCounter(ns); // optional now
      if (typeof updateFilterIconState === 'function') updateFilterIconState(ns);
      if (typeof updateSortIconState === 'function')   updateSortIconState(ns);
      if (typeof renderCustomPager === 'function')     renderCustomPager(ns);
    }, 250);
  }
  tryInit(20); // retry up to ~2 seconds if the DOM isn’t ready yet
});

// Column checkbox change
$(document).on('change', "[id$='colList'] input", function(){

  var id = $(this).closest("[id$='colList']").attr('id');
  var ns = id.replace('colList','');
  var dt = getDT(ns);
  if(!dt) return;

  // Toggle column visibility
  dt.column($(this).data('idx')).visible(this.checked);

  // ---- FIX: update Select All checkbox ----
  var $allCheckboxes = $('#' + ns + 'colList input');
  var total = $allCheckboxes.length;
  var checked = $allCheckboxes.filter(':checked').length;

  $('#' + ns + 'selectAllCols').prop('checked', total === checked);

  setTimeout(function(){
    decorateHeaders(ns);
    disableNativeHeaderSort(ns);
    updateColCounter(ns);
    updateFilterIconState(ns);
    updateSortIconState(ns);
    renderCustomPager(ns);
  }, 50);
});

// Select all columns
$(document).on('change', "input[id$='selectAllCols']", function(){
  if(!this.checked) return; 
  var ns = $(this).attr('id').replace('selectAllCols','');
  var dt = getDT(ns);
  if(!dt) return;
  $('#' + ns + 'colList input').prop('checked', this.checked);
  dt.columns().visible(this.checked);
  setTimeout(function(){ decorateHeaders(ns); disableNativeHeaderSort(ns); updateColCounter(ns); updateFilterIconState(ns); updateSortIconState(ns); renderCustomPager(ns); }, 50);
});

// Reset columns
$(document).on('click', "button[id$='resetCols']", function(){
  var ns = $(this).attr('id').replace('resetCols','');
  var dt = getDT(ns);
  if(!dt) return;
  $('#' + ns + 'colList input').prop('checked', false);
  $('#' + ns + 'selectAllCols').prop('checked', false);
  dt.columns().visible(false);
  setTimeout(function(){ decorateHeaders(ns); disableNativeHeaderSort(ns); updateColCounter(ns); updateFilterIconState(ns); updateSortIconState(ns); renderCustomPager(ns); }, 50);
});

// Column search in overlay
$(document).on('input', "input[id$='colSearch']", function(){
  var ns = $(this).attr('id').replace('colSearch','');
  var q = this.value.toLowerCase();
  $('#' + ns + 'colList .col-item').each(function(){ $(this).toggle($(this).text().toLowerCase().includes(q)); });
});

// Open/close column overlay
$(document).on('click', "button[id$='colCounterSelect']", function(e){
  e.stopPropagation();
  var ns = $(this).attr('id').replace('colCounterSelect','');
  $('#' + ns + 'colOverlay').toggleClass('hidden');
  if($('#' + ns + 'colOverlay').hasClass('hidden')){
    $('#' + ns + 'table').removeClass('blur-body');
  } else {
    $('#' + ns + 'table').addClass('blur-body');
  }
});
$(document).on('click', function(e){
  if(!$(e.target).closest('.col-overlay, button[id$="colCounterSelect"]').length){
    $('.col-overlay').addClass('hidden');
    $('[id$="table"]').removeClass('blur-body');
  }
});

function updateColCounter(ns){
  var dt = getDT(ns);
  if(!dt) return;
  var visible = dt.columns(':visible').count();
  var total   = dt.columns().count();
  $('#' + ns + 'colCountText').text(visible + '/' + total + ' Selected');
  $('#' + ns + 'colCounterSelect span:first').text(visible + ' columns selected');
}

function updateFilterIconState(ns){

  var dt = getDT(ns);
  if(!dt) return;

  var out = $('#' + ns + 'table');
  if(!out.length) return;

  var wrap = out.closest('.dataTables_wrapper');
  var head = wrap.length ? wrap.find('.dataTables_scrollHead thead') : out.find('thead');
  if(!head.length) head = out.find('thead');

  var columnCount = dt.columns().count();

  head.find('tr:first-child th').each(function(i){

    if(i >= columnCount) return;

    var icon = $(this).find('.filter-ico');
    if(!icon.length) return;

    try {
      var val = dt.column(i).search();
      if(val && val.length) {
        icon.addClass('active-filter');
      } else {
        icon.removeClass('active-filter');
      }
    } catch(e){
      console.log("Invalid column index:", i);
    }

  });
}

function updateSortIconState(ns){
  var dt = getDT(ns);
  if(!dt) return;
  var out = $('#' + ns + 'table');
  out.find('.sort-ico').removeClass('active-sort asc desc');
  var ord = dt.settings()[0].aaSorting;
  if(!ord || !ord.length) return;
  var idx = ord[0][0], dir = ord[0][1];
  out.find('.sort-ico[data-idx="' + idx + '"]').addClass('active-sort').addClass(dir);
}

function decorateHeaders(ns){
  var dt = getDT(ns);
  if(!dt) return;
  var wrapper = $('#' + ns + 'table');
  if(!wrapper.length) return;
  
  // Only decorate the visible thead in scrollHead (to avoid duplicates in scrollBody)
  var dtWrapper = wrapper.closest('.dataTables_wrapper');
  var targetThead = dtWrapper.length ? dtWrapper.find('.dataTables_scrollHead thead') : wrapper.find('thead');
  if(!targetThead.length) targetThead = wrapper.find('thead');

  targetThead.each(function(){
    $(this).find('tr:first-child th').each(function(i){
      var th = $(this);
      if(th.find('.th-wrap').length > 0) return;
      var title = th.contents().filter(function(){ return this.nodeType === 3; }).text().trim();
      if(!title) title = th.clone().children().remove().end().text().trim();
      th.contents().filter(function(){ return this.nodeType === 3; }).remove();
      var wrap  = $('<div class="th-wrap"></div>');
      var tspan = $('<span class="th-title"></span>').attr('data-ns', ns).attr('data-idx', i).text(title || '');
      var icons = $('<span class="th-icons"></span>')
        .append('<img class="sort-ico" data-ns="' + ns + '" data-idx="' + i + '" src="images/sort_icon.png" />')
        .append('<img class="filter-ico" data-ns="' + ns + '" data-idx="' + i + '" src="images/filter_icon.png" />');
      wrap.append(tspan).append(icons);
      th.prepend(wrap);
    });
  });
}

$(document).on('mousedown pointerdown', '.dt-filter-popup, .dt-filter-popup *', function(e){ e.stopPropagation(); });

// Sort icon click
$(document).on('click', '.sort-ico', function(e){
  e.preventDefault(); e.stopPropagation(); e.stopImmediatePropagation();
  var ns  = $(this).data('ns');
  var idx = parseInt($(this).data('idx'), 10);
  var dt  = getDT(ns);
  if(!dt) return false;
  var cur = dt.order();
  var curDir = (cur && cur.length && cur[0][0] === idx) ? cur[0][1] : null;
  var nextOrder = curDir === null ? [[idx,'asc']] : curDir === 'asc' ? [[idx,'desc']] : [];
  dt.settings()[0].aaSorting = nextOrder;
  dt.draw(false);
  setTimeout(function(){ updateSortIconState(ns); updateFilterIconState(ns); renderCustomPager(ns); }, 30);
  return false;
});

// Filter icon click -> popup
$(document).on('click', '.filter-ico', function(e){
  e.preventDefault(); e.stopPropagation(); e.stopImmediatePropagation();
  $('.dt-filter-popup').remove();
  var ns  = $(this).data('ns');
  var idx = parseInt($(this).data('idx'), 10);
  var dt  = getDT(ns);
  if(!dt) return false;
  var th  = $(this).closest('th');
  var popup = $('<div class="dt-filter-popup"></div>');
  var input = $('<input type="text" class="dt-filter-input" placeholder="Type &amp; press Enter..." />');
  input.val(dt.column(idx).search());
  popup.append(input);
  $('body').append(popup);
  var offset = th.offset();
  popup.css({ top: (offset.top + th.outerHeight() + 6) + 'px', left: offset.left + 'px' });
  setTimeout(function(){ input.focus().select(); }, 20);
  input.off('keydown').on('keydown', function(ev){
    if(ev.key === 'Enter'){
      ev.preventDefault();
      dt.column(idx).search($(this).val().trim()).draw(false);
      popup.remove();
      setTimeout(function(){ updateFilterIconState(ns); updateSortIconState(ns); renderCustomPager(ns); }, 20);
    }
    if(ev.key === 'Escape') popup.remove();
  });
  setTimeout(function(){
    $(document).off('mousedown.dtFilter').on('mousedown.dtFilter', function(evt){
      if(!$(evt.target).closest('.dt-filter-popup').length){
        $('.dt-filter-popup').remove();
        $(document).off('mousedown.dtFilter');
      }
    });
  }, 50);
  return false;
});


Shiny.addCustomMessageHandler('wirePagerOnce', function(msg){
  // Give DT a moment to finish 'init.dt'
  setTimeout(function(){ renderCustomPager(msg.ns); }, 150);
});

// Custom pagination
function renderCustomPager(ns){

  var dt = getDT(ns);
  dt.columns.adjust();

  var out = $('#' + ns + 'table');
  if(!out.length) return;
  var dtWrap = out.closest('.dataTables_wrapper');
  if(!dtWrap.length) dtWrap = out.parent();
  dtWrap.find('.custom-pager-wrap').remove();
  var info  = dt.page.info();
  var page  = info.page + 1;
  var pages = info.pages;
  var pager = $('<div class="custom-pager-wrap" data-ns="' + ns + '"><div class="pager-center"><button class="pg-btn" data-act="first">&lt;&lt;</button><button class="pg-btn" data-act="prev">&lt;</button><div class="pg-pages"></div><button class="pg-btn" data-act="next">&gt;</button><button class="pg-btn" data-act="last">&gt;&gt;</button><select class="pg-len"><option value="5">5</option><option value="10">10</option><option value="15">15</option><option value="25">25</option><option value="50">50</option></select><div class="pg-jump"><span>Jump to page:</span><input type="number" min="1" class="pg-jump-input" value="' + page + '" /></div></div></div>');
  dtWrap.append(pager);
  var pagesDiv = pager.find('.pg-pages').empty();
  var start = Math.max(1, page - 2), end = Math.min(pages, start + 4);
  start = Math.max(1, end - 4);
  for(var p = start; p <= end; p++){
    var btn = $('<button class="pg-num">' + p + '</button>').attr('data-page', p);
    if(p === page) btn.addClass('active');
    pagesDiv.append(btn);
  }
  pager.find('.pg-len').val(info.length);
}

$(document).on('click', '.custom-pager-wrap .pg-btn', function(){
  var ns = $(this).closest('.custom-pager-wrap').data('ns');
  var dt = getDT(ns); if(!dt) return;
  var act = $(this).data('act');
  if(act === 'first') dt.page('first').draw(false);
  if(act === 'prev')  dt.page('previous').draw(false);
  if(act === 'next')  dt.page('next').draw(false);
  if(act === 'last')  dt.page('last').draw(false);
  setTimeout(function(){ renderCustomPager(ns); }, 20);
});

$(document).on('click', '.custom-pager-wrap .pg-num', function(){
  var ns = $(this).closest('.custom-pager-wrap').data('ns');
  var dt = getDT(ns); if(!dt) return;
  dt.page(parseInt($(this).attr('data-page')) - 1).draw(false);
  setTimeout(function(){ renderCustomPager(ns); }, 20);
});

$(document).on('change', '.custom-pager-wrap .pg-len', function(){
  var ns = $(this).closest('.custom-pager-wrap').data('ns');
  var dt = getDT(ns); if(!dt) return;
  dt.page.len(parseInt($(this).val())).draw(false);
  setTimeout(function(){ renderCustomPager(ns); }, 20);
});

$(document).on('keydown', '.custom-pager-wrap .pg-jump-input', function(e){
  if(e.key !== 'Enter') return;
  var ns = $(this).closest('.custom-pager-wrap').data('ns');
  var dt = getDT(ns); if(!dt) return;
  var pages = dt.page.info().pages;
  var p = Math.max(1, Math.min(parseInt($(this).val()), pages));
  if(!isNaN(p)) dt.page(p - 1).draw(false);
  setTimeout(function(){ renderCustomPager(ns); }, 20);
});

// Force pager refresh on Shiny output updates
$(document).on('shiny:value', function(){
  $('[id$="table"]').each(function(){
    var ns = $(this).attr('id').replace('table','');
    setTimeout(function(){
      var dt = getDT(ns); if(!dt) return;
      decorateHeaders(ns); disableNativeHeaderSort(ns);
      updateFilterIconState(ns); updateSortIconState(ns); renderCustomPager(ns);
    }, 200);
  });
});

$(document).on('init.dt draw.dt', function(e, settings){
  if(!settings || !settings.nTable) return;
  var tableId = settings.nTable.id; if(!tableId) return;
  var ns = tableId.replace(/table_table$/, '');
  setTimeout(function(){
    var dt = getDT(ns); if(!dt) return;
    decorateHeaders(ns); disableNativeHeaderSort(ns);
    updateFilterIconState(ns); updateSortIconState(ns);
    if(e.type === 'draw') renderCustomPager(ns);
  }, 80);
});
