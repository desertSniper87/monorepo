let $itemUploadForm = $('#itemUploadForm');
let $uploadBtn = $('#uploadBtn');
let categorySelectTemplateHtml= $('#categorySelectTemplate').html();
let $categorySelect = $('#categorySelect');
let $buyerOrderTemplateHTML = $('#buyerOrderTemplate').html();
let productCardTemplateHTML = $('#productCardTemplate').html();

$(document).ready(function(){
  if (account_type == "seller") {
    $uploadBtn.removeClass("d-none");
    $.ajax({
      url: config.backend_url + "/products/",
      method: "GET",
      crossDomain: true,
      headers: {
        "Authorization": "Token " + Cookies.get('token'),
      },
      success: function (data, textStatus, jqXHR) {
        console.log(data);
        html = Mustache.to_html(productCardTemplateHTML, {data: data.results});
        $('#orderContainer').html(html);
      },
      error: function (jqXHR, textStatus, errorThrown) {
        console.log("error");
        console.log("textStatus: " + textStatus);
        console.log("errorThrown: " + errorThrown);
      }
    });
  } 
  else if (account_type == "buyer") {
    //Query for order information
    $.ajax({
      url: config.backend_url + "/orders/",
      method: "GET",
      crossDomain: true,
      headers: {
        "Authorization": "Token " + Cookies.get('token'),
      },
      success: function (data, textStatus, jqXHR) {
        console.log(data);
        html = Mustache.to_html($buyerOrderTemplateHTML, {data: data.results});
        $('#orderContainer').html(html);
      },
      error: function (jqXHR, textStatus, errorThrown) {
        console.log("error");
        console.log("textStatus: " + textStatus);
        console.log("errorThrown: " + errorThrown);
      }
    });
  };
});

// Query for categories.
$uploadBtn.click(function(envent){
  $.ajax({
    url: config.backend_url + "/product_categories/",
    method: "GET",
    crossDomain: true,
    success: function (data, textStatus, jqXHR) {
      html = Mustache.to_html(categorySelectTemplateHtml, {data: data.results});
      $categorySelect.html(html);
    },
    error: function (jqXHR, textStatus, errorThrown) {
      console.log("error");
      console.log("textStatus: " + textStatus);
      console.log("errorThrown: " + errorThrown);
    }
  });
});


// Upload object information.
$itemUploadForm.submit(function(event){
  event.preventDefault();
  console.log("submit");

  let $formImageInput = $('#inputImage');
  let formData = new FormData();
  let imageData = $formImageInput[0].files[0];
  formData.append('image', imageData);

  $itemUploadForm.serializeArray().forEach(
    function(element){
      formData.append(element.name, element.value)
    });


  $.ajax({
    url: config.backend_url + "/products/",
    method: "POST",
    data: formData,

    cache: false,
    contentType: false,
    processData: false,

    crossDomain: true,

    // Custom XMLHttpRequest
    xhr: function () {
      var myXhr = $.ajaxSettings.xhr();
      if (myXhr.upload) {
        // For handling the progress of the upload
        myXhr.upload.addEventListener('progress', function (e) {
          if (e.lengthComputable) {
            $('progress').attr({
              value: e.loaded,
              max: e.total,
            });
          }
        }, false);
      }
      return myXhr;
    },

    headers: {
      "Authorization": "Token " + Cookies.get('token'),
    },
    success: function (data, textStatus, jqXHR) {
      console.log("Success");
      $('#uploadFormModal').modal('hide');
    },
    error: function (jqXHR, textStatus, errorThrown) {
      console.log("error");
      console.log("textStatus: " + textStatus);
      console.log("errorThrown: " + errorThrown);
    }
  });
});
