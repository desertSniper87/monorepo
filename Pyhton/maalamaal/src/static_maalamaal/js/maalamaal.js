$(document).ready(function(){
  /*
   *Ajaxify contact form
   */

  var $contactForm = $(".contact-form");
  var contactFormMethod = $contactForm.attr("method");
  var contactFormEndpoint = $contactForm.attr("action");

  function displaySubmitting(submitBtn, defaultText, doSubmit){
    if (doSubmit == true){
      submitBtn.removeClass("disabled");
      submitBtn.html("<i class='fa fa-spin fa-spinner'></i> Searching...");
    } else {
      submitBtn.addClass("disabled");
      submitBtn.html(submitBtnTxt);
    }
  }

  $contactForm.submit(function(event){
    event.preventDefault();

    var contactFormSubmitBtn = $contactForm.find("[type='submit']");
    var contactFormSubmitBtnTxt = contactFormSubmitBtn.text();
    //displaySubmitting(contactFormSubmitBtn, "", true);
    contactFormSubmitBtn.html("<i class='fa fa-spin fa-spinner'></i> Submitting...");

    var contactFormData = $contactForm.serialize();
    var $thisContactForm = $(this);

    $.ajax({
      method: contactFormMethod,
      url: contactFormEndpoint,
      data: contactFormData,

      success: function(data){
        setTimeout(function(){
          //displaySubmitting(contactFormSubmitBtn, contactFormSubmitBtnTxt, false);
          contactFormSubmitBtn.html(contactFormSubmitBtnTxt);
        }, 1000);
        $.alert({
          title: "Success!",
          content: data.message,
          theme: "modern",
        });
        $thisContactForm[0].reset();
      },

      error: function(errorData) {
        console.log("Error: ", errorData.responseJSON);
        var jsonData = errorData.responseJSON;
        var cFormErrMsg = "";

        $.each(jsonData, function(key, value){
          cFormErrMsg += key + ": " + value[0].message + "<br/>";
        });
        //console.log(errorData);
        $.alert({
          title: "Oops!",
          content: cFormErrMsg,
          theme: "modern",
        });
      }

    });
  });

  /*Auto Search*/
  var searchForm = $(".search-form");
  console.log("searchForm.html(): ", searchForm.html());
  var searchInput = searchForm.find("[name='q']");
  console.log("searchInput: ", searchInput);
  var typingTimer;
  var typingInterval = 1000;
  var searchBtn = searchForm.find("[type='submit']");

  searchInput.keyup(function(event){
    clearTimeout(typingTimer); 
    typingTimer = setTimeout(performSearch, typingInterval)
  })

  searchInput.keydown(function(event){
    clearTimeout(typingTimer); 
  })

  function performSearch(){
    displaySubmitting();
    var query = searchInput.val();
    window.location.href = '/search/?q=' + query;
  }

  function displaySubmitting(){
    searchBtn.addClass("disabled");
    searchBtn.html("<i class='fa fa-spin fa-spinner'></i> Searching...");
  }


  /*Cart Add Products*/
  var $product_form = $(".form-product-ajax");

  $product_form.submit(function(event) {
    event.preventDefault();
    console.log("Hello there!");

    var $this_form = $(this);
    var action_endpoint = $this_form.attr("action");
    var http_method = $this_form.attr("method");
    var form_data = $this_form.serialize();

    console.log($this_form.attr("action"), $this_form.attr("method"));
    $.ajax({
      url: action_endpoint,
      method: http_method,
      data: form_data ,

      success: function(data){
        console.log("Top of the morning to you!");
        console.log(data);
        console.log("data.added: ", data.added);
        console.log("data.removed: ", data.removed);

        var submit_span = $this_form.find(".submit-span");
        console.log("submit_span.html(): ", submit_span.html());

        if (data.added){
          submit_span.html("In cart <button class='btn btn-warning' type='submit'>Remove?</button>");
        } else {
          submit_span.html("Not in Cart <button class='btn btn-warning' type='submit' >Add it?</button>");
        }

        var $navBarCount = $('.navbar-cart-count');
        $navBarCount.text(data.cartItemCount);

        var currentPath = window.location.href;
        console.log(currentPath);

        if (currentPath.indexOf("cart") != -1){
          refreshCart();
        }
      },
      error: function(errorData){
        $.alert({
          title: "Oops!",
          content: "An error occured",
          theme: "modern",
        });
        console.log("Error: ");
        console.log(errorData);
      },
    });

  });
});
function refreshCart(event){
  console.log("In current cart");
  var $cartTable = $('.cart-table');
  var cartTBody = $cartTable.find('.cart-table-body');
  //cartTBody.html('<h1>Work in progress</h1>');

  var productRows = cartTBody.find('.cart-product')
  var refreshCartUrl = '/api/cart/';
  var currentUrl = window.location.href

  var refreshCartMethod = 'GET';
  var data;
  $.ajax({

    url: refreshCartUrl,
    method: refreshCartMethod,
    data: data,

    success: function(data){
      /*
       *fallback code
       */
      location.reload();

      /*
       *
       *Start of jquery code
       *
       */

      //var hiddenCartItemRemoveForm = $('.form-product-ajax')
      //console.log("Success"); 
      //console.log("data: ", data);

      //if (data.products.length > 0){

        //i = data.products.length;
        //productRows.html(" ");

        //$.each(data.products, function(index, value){
          //console.log("index: ", index);
          //console.log("value: ", value);
          //var newCartItemRemoveForm = hiddenCartItemRemoveForm.clone();
          //newCartItemRemoveForm.css("display", "block");
          //hiddenCartItemRemoveForm.find(".cart-item-product-id").val(value.id);
          //hiddenCartItemRemoveForm.val(value.id);
          //console.log("hiddenCartItemRemoveForm.html(): ", hiddenCartItemRemoveForm.html());
          //cartTBody.prepend("<tr> \
            //<th scope=\"row\">"+
            //i +
            //"</th> \
            //<td> \
            //<a href='"+ value.url + "'>"+ 
            //value.name +
            //"</a> <div>"+
            ////newCartItemRemoveForm.html()+
            //hiddenCartItemRemoveForm.html()+
            //"</div>"+
            //"</td> \
            //<td>"+
            //value.price +
            //"</td> \
            //</tr>");
            //i--;
          //});
        //cartTBody.find(".cart-total").text(data.total);
        //cartTBody.find(".cart-tax-total").text(data.tax_total);
      //} else {
        //window.location.href = currentUrl;
      //}
    /*
     *End of jquery code
     */

      },
      error: function(errorData){
      console.log("Error: ");
      console.log(errorData);
      },
    });
  }



