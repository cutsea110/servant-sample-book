using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

using ServantClientBook;
using System.Diagnostics;

namespace ConsoleApplication1
{
    class Program
    {
        static void Main(string[] args)
        {
            try
            {
                var api = new API("http://192.168.11.3:8081");

                #region Address Test
                Address address = api.getAddress(1);
                List<Address> addresses = api.getAddresses();
                Address address2 = new Address()
                {
                    addressId = null,
                    postcode = "123-4567",
                    prefecture = Prefecture.Tokyo,
                    address = "船堀1-2-3-404",
                    building = "コナミ船堀",
                    tel = "090-9876-5432",
                    fax = "03-1234-5678",
                    email = "cutsea110@gmail.com",
                    createdAt = DateTime.Now,
                    updatedAt = DateTime.Now
                };
                address2.addressId = api.postAddress(address2);
                address2.email = "cut-sea@timedia.co.jp";
                api.putAddress(address2.addressId.Value, address2);
                api.deleteAddress(address2.addressId.Value);
                #endregion

                #region Author Test
                Author author = api.getAuthor(1);
                AuthorList authors = api.getAuthors();
                AuthorList authors2 = api.postAuthors(new AuthorQueryCondition()
                {
                    genderEq=Gender.Female,
                    authorNameLike="伊東",
                    prefectureIn = new List<Prefecture>()
                    {
                        Prefecture.Tochigi,Prefecture.Saga
                    }
                });
                Author author2 = new Author()
                {
                    authorId = null,
                    name = "伊東 勝利",
                    age = 45,
                    gender = Gender.Male,
                    birth = new DateTime(1970, 11, 6),
                    address = address2,
                    createdAt = DateTime.Now,
                    updatedAt = DateTime.Now
                };
                author2.authorId = api.postAuthor(author2);
                author2.name = "伊東 奈緒";
                author2.birth = new DateTime(2012, 11, 2);
                author2.age = 3;
                api.putAuthor(author2.authorId.Value, author2);
                api.deleteAuthor(author2.authorId.Value);
                #endregion

                #region Publisher Test
                Publisher publissher = api.getPublisher(1);
                List<Publisher> publishers = api.getPublishers();
                Publisher publisher2 = new Publisher()
                {
                    publisherId = null,
                    name = "オーム社",
                    companyType = CompanyType.CO,
                    address = address2,
                    createdAt = DateTime.Now,
                    updatedAt = DateTime.Now
                };
                publisher2.publisherId = api.postPublisher(publisher2);
                publisher2.name = "オライリー";
                publisher2.companyType = CompanyType.INC;
                publisher2.address = address;
                api.putPublisher(publisher2.publisherId.Value, publisher2);
                api.deletePublisher(publisher2.publisherId.Value);
                #endregion

                #region Book Test
                Book book = api.getBook(1);
                BookList books = api.getBooks();
                BookList books2 = api.postBooks(new BookQueryCondition()
                {
                    categoryIn = new List<Category>()
                    {
                        Category.Science,
                        Category.Comics
                    },
                    publishedFrom = new DateTime(2010, 1, 1),
                    publishedTo = new DateTime(2016, 12, 31)
                });
                Book book2 = new Book()
                {
                    bookId = null,
                    title = "Real World Haskell",
                    description = "For Haskellers",
                    category = Category.Computer,
                    isbn = "ISBN123-4567-890-123",
                    authors = new List<AuthorInfo>() {
                    new AuthorInfo()
                    {
                        authorId=1,
                        name="伊東 勝利"
                    },
                    new AuthorInfo()
                    {
                        authorId=1,
                        name="山下 伸夫"
                    }
                },
                    publishedBy = new PublisherInfo()
                    {
                        publisherId = 3,
                        name = "オーム社"
                    },
                    publishedAt = new DateTime(2000, 1, 1),
                    createdAt = DateTime.Now,
                    updatedAt = DateTime.Now
                };
                book2.bookId = api.postBook(book2);
                book2.title = book2.title + " Ver.2";
                api.putBook(book2.bookId.Value, book2);
                api.deleteBook(book2.bookId.Value);

                book = api.getBook("ISBN123-4567-8901-123");
                api.putBook(book2.isbn, book2);
                api.deleteBook(book2.isbn);

                #endregion

            }catch(Exception e)
            {
                Debug.WriteLine(e.Message);
            }
        }
    }
}
