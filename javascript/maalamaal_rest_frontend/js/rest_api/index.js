let productCardTemplateHtml= $('#productCardTemplate').html();
let cartTemplateHtml= $('#cartTemplate').html();
let $productList = $('#productList');
let $categoryList = $('#categoryList');
let categoryListTemplateHtml= $('#categoryListTemplate').html();

var queryForProduct = function(categoryID){
  $.ajax({
      url: config.backend_url + "/products/",
      method: "GET",
      crossDomain: true,
      data: {category: categoryID},
      success: function (data, textStatus, jqXHR) {
          globalProductData = data.results;
          html = Mustache.to_html(productCardTemplateHtml, {data: data.results});
          $productList.html(html);
          $('#loadingSpinner').hide();
      },
      error: function (jqXHR, textStatus, errorThrown) {
          $('#loadingSpinner').hide();
          $('#serverErrorMsg').show();
          console.log("error");
          console.log("textStatus: " + textStatus);
          console.log("errorThrown: " + errorThrown);
      },
      timeout: 10000
  });
}

// Query for products (and cart).
$(document).ready(function(){
    if (getCartStatusInCookie) {
        $('#cartButton').removeClass('d-none');
    }
    $('#cartButton').removeClass('d-none');

    $.ajax({
        url: config.backend_url + "/products/",
        method: "GET",
        crossDomain: true,
        success: function (data, textStatus, jqXHR) {
            globalProductData = data.results;
            html = Mustache.to_html(productCardTemplateHtml, {data: data.results});
            $productList.append(html);
            $('#loadingSpinner').hide();
        },
        error: function (jqXHR, textStatus, errorThrown) {
            $('#loadingSpinner').hide();
            $('#serverErrorMsg').show();
            console.log("error");
            console.log("textStatus: " + textStatus);
            console.log("errorThrown: " + errorThrown);
        },
        timeout: 10000
    });

    $.ajax({
      url: config.backend_url + "/product_categories/",
      method: "GET",
      crossDomain: true,
      success: function (data, textStatus, jqXHR) {
        html = Mustache.to_html(categoryListTemplateHtml, {data: data.results});
        $categoryList.html(html);
      },
      error: function (jqXHR, textStatus, errorThrown) {
        console.log("error");
        console.log("textStatus: " + textStatus);
        console.log("errorThrown: " + errorThrown);
      }
    });
});

$(document).on("click", ".categoryBtn", function(event){
  event.preventDefault();
  queryForProduct($(this).attr('value'));
});

let $productDetailsModal= $('#productDetailsModal');
let $btnProductDetails = $('.btnProductDetails');
var currentlySelectedProductId;
var currentlySelectedProductUnitPrice;

var getCurrentProduct = function (currentlySelectedProductId){ 
    let currentProduct = globalProductData.filter(obj => {
      return obj.id === currentlySelectedProductId;
    })[0];

    return currentProduct;
}

$(document).on("click", ".btnProductDetails", function(){
    currentlySelectedProductId = Number($(this).attr('data-itemId'));
    currentProduct = getCurrentProduct(currentlySelectedProductId);

    $('#productDetailsCardImage').attr('src', currentProduct.image);
    $('#modalProductName').text(currentProduct.name);
    $('#modalProductPrice').text('Per unit price: ' + currentProduct.price_taka + ' Taka');
    $('#modalInputQuantity').attr('max', currentProduct.available_quantity);
    $('#modalInputProductUrl').val(config.backend_url + "/products/" + currentProduct.id + "/");
    $('#modalAvailableQuantity').text(currentProduct.available_quantity + 'pc. Available');
    $('#modalProductSeller').text('Seller name: ' + currentProduct.seller);
    $('#modalProductDesc').text(currentProduct.description);

    $productDetailsModal.modal('show');
});

$(document).on("change", "#modalInputQuantity", function(){
    currentProduct = getCurrentProduct(currentlySelectedProductId)
    total_cost = currentProduct.price_taka * $(this).val();
    $('#modalTotalPrice').text(total_cost);
    $('#modalTotalCostText').removeClass('d-none');
});

var getCartData = function (){ 
    let result;
    $.ajax({
        url: config.backend_url + "/carts/",
        data: $(this).serialize(),
        // dataType
        method: "GET",
        crossDomain: true,
        async: false,
        headers: {
            "Authorization": "Token " + Cookies.get('token'),
        },
        success: function (data, textStatus, jqXHR) {
            //$('#cartButton').removeClass('d-none');
            result = data;
        },
        error: function (jqXHR, textStatus, errorThrown) {
            console.log("error");
            console.log("textStatus: " + textStatus);
            console.log("errorThrown: " + errorThrown);
        },
    });
    return result;
};

var setCartStatusInCookie = function(val){
    username = Cookies.get('username');
    Cookies.set(username + '-cart', val);
}

var getCartStatusInCookie = function(){
    username = Cookies.get('username');
    return Cookies.get(username + '-cart');
}

// Cart creation.
$(document).ready(function(){
    $('#cartCreationForm').submit(function(event){
        event.preventDefault();
        $.ajax({
            url: config.backend_url + "/carts/",
            data: $(this).serialize(),
            // dataType
            method: "POST",
            crossDomain: true,
            headers: {
                "Authorization": "Token " + Cookies.get('token'),
            },
            success: function (data, textStatus, jqXHR) {
                //getCartData();
                setCartStatusInCookie(true);
                $productDetailsModal.modal('hide');

            },
            error: function (jqXHR, textStatus, errorThrown) {
                console.log("error");
                console.log("textStatus: " + textStatus);
                console.log("errorThrown: " + errorThrown);
            },
        });
    });

    // Show cart
    $('#cartButton').click(function(event){
        event.preventDefault();

        data = getCartData();
        var idx = 1;
        html = Mustache.to_html(cartTemplateHtml, {
            data: data.results,
            idx: function(){ return idx++; 
        }});
        $('#cartDetailsModal').modal('show');
        $('#cartDetailModalBody').html(html);
    });

    // Checkout
    $('#btnChkout').click(function(){
        event.preventDefault();
        $.ajax({
            url: config.backend_url + "/orders/",
            data: $(this).serialize(),
            // dataType
            method: "POST",
            crossDomain: true,
            headers: {
                "Authorization": "Token " + Cookies.get('token'),
            },
            success: function (data, textStatus, jqXHR) {
                console.log(data);
                setCartStatusInCookie(false);
                window.location.href = '/profile.html';
            },
            error: function (jqXHR, textStatus, errorThrown) {
                console.log("error");
                console.log("textStatus: " + textStatus);
                console.log("errorThrown: " + errorThrown);
            },
        });
    })
});

$( document ).ajaxStop(function() {
    if (Cookies.get('account_type')  == "seller") {
      console.log('Bullets dont work jon');
      $('.card-footer').hide();
    }
});
