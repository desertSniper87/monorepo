from django.shortcuts import render
from django.views.generic import ListView, TemplateView
from .models import Fertilizer_shop

# from rest_framework.views import APIView
from rest_framework import viewsets
from .serializers import FertilizerShopSerlializer

# Create your views here.
class ShopListView(ListView):
    template_name = "fertilizer_shops/list.html"

    def get_queryset(self, *args, **kwargs):
        request = self.request
        return Fertilizer_shop.objects.all()

# class ShopListAPIView(APIView):
    # def get(self, request, format=None):
            # """
            # Return a list of all users.
            # """
            # shops = Fertilizer_shop.objects.all()
            # return Response(shops)
class ShopListAPIViewSet(viewsets.ModelViewSet):
    """
    API endpoint that allows users to be viewed or edited.
    """
    queryset = Fertilizer_shop.objects.all()
    serializer_class = FertilizerShopSerlializer

class FindMapView(TemplateView):
    template_name = "fertilizer_shops/find_map.html"

    # def get_queryset(self, *args, **kwargs):
        # request = self.request
        # return Fertilizer_shop.objects.all()

