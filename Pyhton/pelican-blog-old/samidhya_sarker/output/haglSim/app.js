var x;
var actualPrice;

function getRandomInt(max) {
  return Math.max(Math.floor(Math.random() * Math.floor(max)), 1);
}

new Vue({
  el: '#app',
  data: {
    gameRunning: false,
    shopPrice: x,
    myPrice: x,
    actualPrice: x,
    tries: getRandomInt(4) + 3,
  },

  methods: {
    startGame: function(){
      this.gameIsRunning = true;
      console.log("Heelo THere");
      x = getRandomInt(100) * 100;
      this.shopPrice= x;
      this.myPrice= x;
      this.actualPrice= (x * (100-getRandomInt(50)))/100;
      this.tries= getRandomInt(4) + 3
    },

    deductedPrice: function(percent){
      return Math.floor((this.myPrice *(100 - percent) /100) /100) * 100;
    },

    askPrice: function(percent){
      var askingPrice = this.deductedPrice(percent);
      console.log("askingPrice: ", askingPrice);
      if (askingPrice>=this.actualPrice && this.tries==0){
        this.deal();
      }
      else if (this.tries==0){
        alert('অনুগ্রহ করে রাস্তা মাপুন।');
        this.startGame();
      }
      else if (askingPrice<this.actualPrice){
        this.tries--;
        this.noDeal();
      }
      else if (askingPrice==this.actualPrice){
        this.deal();
      }
      else {
        this.shopPrice = Math.max((100-getRandomInt(5)) * askingPrice /100, this.actualPrice);
        this.myPrice = askingPrice;
        this.tries--;
      }
    },

    deal: function(){
      if(confirm('ধন্যবাদ। আপনি জিনিসটি ক্রয় করতে সক্ষম হয়েছেন। আবার খেলতে ইচ্ছুক?')){
        this.startGame();
      } else {
        this.gameIsRunning = false;
      }
    },

    noDeal: function(){
     	alert('দুঃখিত। আপনি অধিক কম বলেছেন।');
      }
    },
});
