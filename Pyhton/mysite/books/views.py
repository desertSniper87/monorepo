from django.shortcuts import render
from django.http import HttpResponse
from books.models import Book
from django.views.generic import ListView
from books.models import Publisher



def search_form(request):
    return render(request, 'search_form.html')


def search(request):
    # if 'q' in request.GET and request.GET['q']:
    #     q = request.GET['q']
    #     books = Book.objects.filter(title__icontains=q)
    #     return render(request, 'search_results.html',
    #                   {'books': books, 'query': q})

    # else:
    #     return render(request, 'search_form.html', {'error': True})

    error = False

    if 'q' in request.GET:
        q = request.GET['q']

    if not q:
        error = True
    else:
            books = Book.objects.filter(title__icontains=q)
            return render(request, 'search_results.html',
                          {'books': books, 'query': q})

        # message = 'You searched for %r' % request.GET['q']

    return render(request, 'search_form.html', {'error': error})
        #return HttpResponse('You submitted an empty form')
        # message = 'You submitted an empty form'


    # return HttpResponse(message)http://127.0.0.1:8000/time/

class PublisherList(ListView):
    model = Publisher
    context_object_name = 'fake_news_publishers'

