from django.test import TestCase
import datetime
from django.utils import timezone
from .models import Book

class BookMethodTests(TestCase):
    def test_recent_pub(self):
        """Blah Blah Blah ... Blah Blah Blah """

        futureDate = timezone.now().date() + datetime.timedelta(days=5)
        futurePublication = Book(publication_date=futureDate)
        self.assertEqual(futurePublication.recent_publication(), False)