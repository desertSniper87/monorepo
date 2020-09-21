from django.shortcuts import render
from django.http import JsonResponse, HttpResponseBadRequest

def home(request):
    return render(request, "myapp/home.html", {})


def handle_ajax(request):
    if not request.is_ajax():
        return HttpResponseBadRequest()
    return JsonResponse({'content': "Hi {}, how are you?".format(request.POST.get("name"))}) 

