using Newtonsoft.Json;
using System.Collections.Generic;
using System.Diagnostics;
using System.Net.Http;
using System.Net.Http.Headers;
using System.Text;
using System.Threading.Tasks;

#region type alias
using AddressId = System.Int64;
using AuthorId = System.Int64;
using PublisherId = System.Int64;
using BookId = System.Int64;
using ISBN = System.String;
using Postcode = System.String;
using Tel = System.String;
using Fax = System.String;
using Emailaddress = System.String;
using System.Linq;
#endregion


namespace ServantClientBook
{
    class ServantClient : HttpClient
    {
        public ServantClient()
        {
            this.DefaultRequestHeaders.Accept.Add(new MediaTypeWithQualityHeaderValue("application/json"));
        }
    }
    public class API
    {
        #region Constructor
        static string server;
        public API(string _server)
        {
            server = _server;
        }
        #endregion
        #region Address
        public async Task<Address> getAddressAsync(AddressId addressId)
        {
            var client = new ServantClient();
            var res = await client.GetAsync($"{server}/address/{addressId}");
            Debug.WriteLine($">>> {res.RequestMessage}");
            Debug.WriteLine($"<<< {(int)res.StatusCode} {res.ReasonPhrase}");
            var content = await res.Content.ReadAsStringAsync();
            Debug.WriteLine($"<<< {content}");
            return JsonConvert.DeserializeObject<Address>(content);
        }
        public Address getAddress(AddressId addressId)
        {
            Task<Address> t = getAddressAsync(addressId);
            return t.GetAwaiter().GetResult();
        }
        public async Task<List<Address>> getAddressesAsync(int? page=null, int? per_page=null)
        {
            var client = new ServantClient();
            var queryparams = new List<string> {
                page.HasValue ? $"page={page.Value}" : null,
                per_page.HasValue ? $"per_page={per_page.Value}" : null
            }.Where(e => !string.IsNullOrEmpty(e));
            var qp = queryparams.Count() > 0 ? $"?{string.Join("&", queryparams)}" : "";
            var res = await client.GetAsync($"{server}/addresses{qp}");
            Debug.WriteLine($">>> {res.RequestMessage}");
            Debug.WriteLine($"<<< {(int)res.StatusCode} {res.ReasonPhrase}");
            var content = await res.Content.ReadAsStringAsync();
            Debug.WriteLine($"<<< {content}");
            return JsonConvert.DeserializeObject<List<Address>>(content);
        }
        public List<Address> getAddresses(int? page = null, int? per_page = null)
        {
            Task<List<Address>> t = getAddressesAsync(page, per_page);
            return t.GetAwaiter().GetResult();
        }
        public async Task<int> postAddressAsync(Address obj)
        {
            var client = new ServantClient();
#if DEBUG
            var jsonObj = JsonConvert.SerializeObject(obj, Formatting.Indented);
#else
            var jsonObj = JsonConvert.SerializeObject(obj);
#endif
            var res = await client.PostAsync($"{server}/address", new StringContent(jsonObj, Encoding.UTF8, "application/json"));
            Debug.WriteLine($">>> {res.RequestMessage}");
            Debug.WriteLine($"-----\n{jsonObj}\n-----");
            Debug.WriteLine($"<<< {(int)res.StatusCode} {res.ReasonPhrase}");
            var content = await res.Content.ReadAsStringAsync();
            Debug.WriteLine($"<<< {content}");
            return JsonConvert.DeserializeObject<int>(content);
        }
        public AddressId postAddress(Address obj)
        {
            Task<int> t = postAddressAsync(obj);
            return t.GetAwaiter().GetResult();
        }
        public async Task putAddressAsync(AddressId addressId, Address obj)
        {
            var client = new ServantClient();
#if DEBUG
            var jsonObj = JsonConvert.SerializeObject(obj, Formatting.Indented);
#else
            var jsonObj = JsonConvert.SerializeObject(obj);
#endif
            var res = await client.PutAsync($"{server}/address/{addressId}", new StringContent(jsonObj, Encoding.UTF8, "application/json"));
            Debug.WriteLine($">>> {res.RequestMessage}");
            Debug.WriteLine($"-----\n{jsonObj}\n-----");
            Debug.WriteLine($"<<< {(int)res.StatusCode} {res.ReasonPhrase}");
            var content = await res.Content.ReadAsStringAsync();
            Debug.WriteLine($"<<< {content}");
            JsonConvert.DeserializeObject(content);
        }
        public void putAddress(AddressId addressid, Address obj)
        {
            Task t = putAddressAsync(addressid, obj);
            t.GetAwaiter().GetResult();
        }
        public async Task deleteAddressAsync(AddressId addressId)
        {
            var client = new ServantClient();
            var res = await client.DeleteAsync($"{server}/address/{addressId}");
            Debug.WriteLine($">>> {res.RequestMessage}");
            Debug.WriteLine($"<<< {(int)res.StatusCode} {res.ReasonPhrase}");
            var content = await res.Content.ReadAsStringAsync();
            Debug.WriteLine($"<<< {content}");
            JsonConvert.DeserializeObject(content);
        }
        public void deleteAddress(AddressId addressId)
        {
            Task t = deleteAddressAsync(addressId);
            t.GetAwaiter().GetResult();
        }
#endregion
        #region Author
        public async Task<Author> getAuthorAsync(AuthorId authorId)
        {
            var client = new ServantClient();
            var res = await client.GetAsync($"{server}/author/{authorId}");
            Debug.WriteLine($">>> {res.RequestMessage}");
            Debug.WriteLine($"<<< {(int)res.StatusCode} {res.ReasonPhrase}");
            var content = await res.Content.ReadAsStringAsync();
            Debug.WriteLine($"<<< {content}");
            return JsonConvert.DeserializeObject<Author>(content);
        }
        public Author getAuthor(AuthorId authorId)
        {
            Task<Author> t = getAuthorAsync(authorId);
            return t.GetAwaiter().GetResult();
        }
        public async Task<AuthorList> getAuthorsAsync(int? page=null, int? per_page=null)
        {
            var client = new ServantClient();
            var queryparams = new List<string> {
                page.HasValue ? $"page={page.Value}" : null,
                per_page.HasValue ? $"per_page={per_page.Value}" : null
            }.Where(e => !string.IsNullOrEmpty(e));
            var qp = queryparams.Count() > 0 ? $"?{string.Join("&", queryparams)}" : "";
            var res = await client.GetAsync($"{server}/authors{qp}");
            Debug.WriteLine($">>> {res.RequestMessage}");
            Debug.WriteLine($"<<< {(int)res.StatusCode} {res.ReasonPhrase}");
            var content = await res.Content.ReadAsStringAsync();
            Debug.WriteLine($"<<< {content}");
            return JsonConvert.DeserializeObject<AuthorList>(content);
        }
        public AuthorList getAuthors(int? page = null, int? per_page = null)
        {
            Task<AuthorList> t = getAuthorsAsync(page, per_page);
            return t.GetAwaiter().GetResult();
        }
        public async Task<AuthorList> postAuthorsAsync(AuthorQueryCondition obj)
        {
            var client = new ServantClient();
#if DEBUG
            var jsonObj = JsonConvert.SerializeObject(obj, Formatting.Indented);
#else
            var jsonObj = JsonConvert.SerializeObject(obj);
#endif
            var res = await client.PostAsync($"{server}/authors", new StringContent(jsonObj, Encoding.UTF8, "application/json"));
            Debug.WriteLine($">>> {res.RequestMessage}");
            Debug.WriteLine($"-----\n{jsonObj}\n-----");
            Debug.WriteLine($"<<< {(int)res.StatusCode} {res.ReasonPhrase}");
            var content = await res.Content.ReadAsStringAsync();
            Debug.WriteLine($"<<< {content}");
            return JsonConvert.DeserializeObject<AuthorList>(content);
        }
        public AuthorList postAuthors(AuthorQueryCondition obj)
        {
            Task<AuthorList> t = postAuthorsAsync(obj);
            return t.GetAwaiter().GetResult();
        }
        public async Task<AuthorId> postAuthorAsync(Author obj)
        {
            var client = new ServantClient();
#if DEBUG
            var jsonObj = JsonConvert.SerializeObject(obj, Formatting.Indented);
#else
            var jsonObj = JsonConvert.SerializeObject(obj);
#endif
            var res = await client.PostAsync($"{server}/author", new StringContent(jsonObj, Encoding.UTF8, "application/json"));
            Debug.WriteLine($">>> {res.RequestMessage}");
            Debug.WriteLine($"-----\n{jsonObj}\n-----");
            Debug.WriteLine($"<<< {(int)res.StatusCode} {res.ReasonPhrase}");
            var content = await res.Content.ReadAsStringAsync();
            Debug.WriteLine($"<<< {content}");
            return JsonConvert.DeserializeObject<int>(content);
        }
        public AuthorId postAuthor(Author obj)
        {
            Task<AuthorId> t = postAuthorAsync(obj);
            return t.GetAwaiter().GetResult();
        }
        public async Task putAuthorAsync(AuthorId authorId, Author obj)
        {
            var client = new ServantClient();
#if DEBUG
            var jsonObj = JsonConvert.SerializeObject(obj, Formatting.Indented);
#else
            var jsonObj = JsonConvert.SerializeObject(obj);
#endif
            var res = await client.PutAsync($"{server}/author/{authorId}", new StringContent(jsonObj, Encoding.UTF8, "application/json"));
            Debug.WriteLine($">>> {res.RequestMessage}");
            Debug.WriteLine($"-----\n{jsonObj}\n-----");
            Debug.WriteLine($"<<< {(int)res.StatusCode} {res.ReasonPhrase}");
            var content = await res.Content.ReadAsStringAsync();
            Debug.WriteLine($"<<< {content}");
            JsonConvert.DeserializeObject(content);
        }
        public void putAuthor(AuthorId authorId, Author obj)
        {
            Task t = putAuthorAsync(authorId, obj);
            t.GetAwaiter().GetResult();
        }
        public async Task deleteAuthorAsync(AuthorId authorId)
        {
            var client = new ServantClient();
            var res = await client.DeleteAsync($"{server}/author/{authorId}");
            Debug.WriteLine($">>> {res.RequestMessage}");
            Debug.WriteLine($"<<< {(int)res.StatusCode} {res.ReasonPhrase}");
            var content = await res.Content.ReadAsStringAsync();
            Debug.WriteLine($"<<< {content}");
            JsonConvert.DeserializeObject(content);
        }
        public void deleteAuthor(AuthorId authorId)
        {
            Task t = deleteAuthorAsync(authorId);
            t.GetAwaiter().GetResult();
        }
        #endregion
        #region Publisher
        public async Task<Publisher> getPublisherAsync(AuthorId publisherId)
        {
            var client = new ServantClient();
            var res = await client.GetAsync($"{server}/publisher/{publisherId}");
            Debug.WriteLine($">>> {res.RequestMessage}");
            Debug.WriteLine($"<<< {(int)res.StatusCode} {res.ReasonPhrase}");
            var content = await res.Content.ReadAsStringAsync();
            Debug.WriteLine($"<<< {content}");
            return JsonConvert.DeserializeObject<Publisher>(content);
        }
        public Publisher getPublisher(AuthorId publisherId)
        {
            Task<Publisher> t = getPublisherAsync(publisherId);
            return t.GetAwaiter().GetResult();
        }
        public async Task<PublisherList> getPublishersAsync(int? page=null, int? per_page=null)
        {
            var client = new ServantClient();
            var queryparams = new List<string> {
                page.HasValue ? $"page={page.Value}" : null,
                per_page.HasValue ? $"per_page={per_page.Value}" : null
            }.Where(e => !string.IsNullOrEmpty(e));
            var qp = queryparams.Count() > 0 ? $"?{string.Join("&", queryparams)}" : "";
            var res = await client.GetAsync($"{server}/publishers{qp}");
            Debug.WriteLine($">>> {res.RequestMessage}");
            Debug.WriteLine($"<<< {(int)res.StatusCode} {res.ReasonPhrase}");
            var content = await res.Content.ReadAsStringAsync();
            Debug.WriteLine($"<<< {content}");
            return JsonConvert.DeserializeObject<PublisherList>(content);
        }
        public PublisherList getPublishers(int? page=null, int? per_page=null)
        {
            Task<PublisherList> t = getPublishersAsync(page, per_page);
            return t.GetAwaiter().GetResult();
        }
        public async Task<PublisherList> postPublishersAsync(PublisherQueryCondition obj)
        {
            var client = new ServantClient();
#if DEBUG
            var jsonObj = JsonConvert.SerializeObject(obj, Formatting.Indented);
#else
            var jsonObj = JsonConvert.SerializeObject(obj);
#endif
            var res = await client.PostAsync($"{server}/publishers", new StringContent(jsonObj, Encoding.UTF8, "application/json"));
            Debug.WriteLine($">>> {res.RequestMessage}");
            Debug.WriteLine($"-----\n{jsonObj}\n-----");
            Debug.WriteLine($"<<< {(int)res.StatusCode} {res.ReasonPhrase}");
            var content = await res.Content.ReadAsStringAsync();
            Debug.WriteLine($"<<< {content}");
            return JsonConvert.DeserializeObject<PublisherList>(content);
        }
        public PublisherList postPublishers(PublisherQueryCondition obj)
        {
            Task<PublisherList> t = postPublishersAsync(obj);
            return t.GetAwaiter().GetResult();
        }
        public async Task<PublisherId> postPublisherAsync(Publisher obj)
        {
            var client = new ServantClient();
#if DEBUG
            var jsonObj = JsonConvert.SerializeObject(obj, Formatting.Indented);
#else
            var jsonObj = JsonConvert.SerializeObject(obj);
#endif
            var res = await client.PostAsync($"{server}/publisher", new StringContent(jsonObj, Encoding.UTF8, "application/json"));
            Debug.WriteLine($">>> {res.RequestMessage}");
            Debug.WriteLine($"-----\n{jsonObj}\n-----");
            Debug.WriteLine($"<<< {(int)res.StatusCode} {res.ReasonPhrase}");
            var content = await res.Content.ReadAsStringAsync();
            Debug.WriteLine($"<<< {content}");
            return JsonConvert.DeserializeObject<int>(content);
        }
        public PublisherId postPublisher(Publisher obj)
        {
            Task<PublisherId> t = postPublisherAsync(obj);
            return t.GetAwaiter().GetResult();
        }
        public async Task putPublisherAsync(PublisherId publisherId, Publisher obj)
        {
            var client = new ServantClient();
#if DEBUG
            var jsonObj = JsonConvert.SerializeObject(obj, Formatting.Indented);
#else
            var jsonObj = JsonConvert.SerializeObject(obj);
#endif
            var res = await client.PutAsync($"{server}/publisher/{publisherId}", new StringContent(jsonObj, Encoding.UTF8, "application/json"));
            Debug.WriteLine($">>> {res.RequestMessage}");
            Debug.WriteLine($"-----\n{jsonObj}\n-----");
            Debug.WriteLine($"<<< {(int)res.StatusCode} {res.ReasonPhrase}");
            var content = await res.Content.ReadAsStringAsync();
            Debug.WriteLine($"<<< {content}");
            JsonConvert.DeserializeObject(content);
        }
        public void putPublisher(PublisherId publisherId, Publisher obj)
        {
            Task t = putPublisherAsync(publisherId, obj);
            t.GetAwaiter().GetResult();
        }
        public async Task deletePublisherAsync(PublisherId publisherId)
        {
            var client = new ServantClient();
            var res = await client.DeleteAsync($"{server}/publisher/{publisherId}");
            Debug.WriteLine($">>> {res.RequestMessage}");
            Debug.WriteLine($"<<< {(int)res.StatusCode} {res.ReasonPhrase}");
            var content = await res.Content.ReadAsStringAsync();
            Debug.WriteLine($"<<< {content}");
            JsonConvert.DeserializeObject(content);
        }
        public void deletePublisher(PublisherId publisherId)
        {
            Task t = deletePublisherAsync(publisherId);
            t.GetAwaiter().GetResult();
        }
        #endregion
        #region Book
        public async Task<Book> getBookAsync(BookId bookId)
        {
            var client = new ServantClient();
            var res = await client.GetAsync($"{server}/book/{bookId}");
            Debug.WriteLine($">>> {res.RequestMessage}");
            Debug.WriteLine($"<<< {(int)res.StatusCode} {res.ReasonPhrase}");
            var content = await res.Content.ReadAsStringAsync();
            Debug.WriteLine($"<<< {content}");
            return JsonConvert.DeserializeObject<Book>(content);
        }
        public Book getBook(BookId bookId)
        {
            Task<Book> t = getBookAsync(bookId);
            return t.GetAwaiter().GetResult();
        }
        public async Task<Book> getBookAsync(ISBN isbn)
        {
            var client = new ServantClient();
            var res = await client.GetAsync($"{server}/book/isbn/{isbn}");
            Debug.WriteLine($">>> {res.RequestMessage}");
            Debug.WriteLine($"<<< {(int)res.StatusCode} {res.ReasonPhrase}");
            var content = await res.Content.ReadAsStringAsync();
            Debug.WriteLine($"<<< {content}");
            return JsonConvert.DeserializeObject<Book>(content);
        }
        public Book getBook(ISBN isbn)
        {
            Task<Book> t = getBookAsync(isbn);
            return t.GetAwaiter().GetResult();
        }
        public async Task<BookList> getBooksAsync(int? page=null, int? per_page=null)
        {
            var client = new ServantClient();
            var queryparams = new List<string> {
                page.HasValue ? $"page={page.Value}" : null,
                per_page.HasValue ? $"per_page={per_page.Value}" : null
            }.Where(e => !string.IsNullOrEmpty(e));
            var qp = queryparams.Count() > 0 ? $"?{string.Join("&", queryparams)}" : "";
            var res = await client.GetAsync($"{server}/books{qp}");
            Debug.WriteLine($">>> {res.RequestMessage}");
            Debug.WriteLine($"<<< {(int)res.StatusCode} {res.ReasonPhrase}");
            var content = await res.Content.ReadAsStringAsync();
            Debug.WriteLine($"<<< {content}");
            return JsonConvert.DeserializeObject<BookList>(content);
        }
        public BookList getBooks(int? page = null, int? per_page = null)
        {
            Task<BookList> t = getBooksAsync(page, per_page);
            return t.GetAwaiter().GetResult();
        }
        public async Task<BookList> postBooksAsync(BookQueryCondition obj)
        {
            var client = new ServantClient();
#if DEBUG
            var jsonObj = JsonConvert.SerializeObject(obj, Formatting.Indented);
#else
            var jsonObj = JsonConvert.SerializeObject(obj);
#endif
            var res = await client.PostAsync($"{server}/books", new StringContent(jsonObj, Encoding.UTF8, "application/json"));
            Debug.WriteLine($">>> {res.RequestMessage}");
            Debug.WriteLine($"-----\n{jsonObj}\n-----");
            Debug.WriteLine($"<<< {(int)res.StatusCode} {res.ReasonPhrase}");
            var content = await res.Content.ReadAsStringAsync();
            Debug.WriteLine($"<<< {content}");
            return JsonConvert.DeserializeObject<BookList>(content);
        }
        public BookList postBooks(BookQueryCondition obj)
        {
            Task<BookList> t = postBooksAsync(obj);
            return t.GetAwaiter().GetResult();
        }
        public async Task<BookId> postBookAsync(Book obj)
        {
            var client = new ServantClient();
#if DEBUG
            var jsonObj = JsonConvert.SerializeObject(obj, Formatting.Indented);
#else
            var jsonObj = JsonConvert.SerializeObject(obj);
#endif
            var res = await client.PostAsync($"{server}/book", new StringContent(jsonObj, Encoding.UTF8, "application/json"));
            Debug.WriteLine($">>> {res.RequestMessage}");
            Debug.WriteLine($"-----\n{jsonObj}\n-----");
            Debug.WriteLine($"<<< {(int)res.StatusCode} {res.ReasonPhrase}");
            var content = await res.Content.ReadAsStringAsync();
            Debug.WriteLine($"<<< {content}");
            return JsonConvert.DeserializeObject<int>(content);
        }
        public BookId postBook(Book obj)
        {
            Task<BookId> t = postBookAsync(obj);
            return t.GetAwaiter().GetResult();
        }
        public async Task putBookAsync(BookId bookId, Book obj)
        {
            var client = new ServantClient();
#if DEBUG
            var jsonObj = JsonConvert.SerializeObject(obj, Formatting.Indented);
#else
            var jsonObj = JsonConvert.SerializeObject(obj);
#endif
            var res = await client.PutAsync($"{server}/book/{bookId}", new StringContent(jsonObj, Encoding.UTF8, "application/json"));
            Debug.WriteLine($">>> {res.RequestMessage}");
            Debug.WriteLine($"-----\n{jsonObj}\n-----");
            Debug.WriteLine($"<<< {(int)res.StatusCode} {res.ReasonPhrase}");
            var content = await res.Content.ReadAsStringAsync();
            Debug.WriteLine($"<<< {content}");
            JsonConvert.DeserializeObject(content);
        }
        public void putBook(BookId bookId, Book obj)
        {
            Task t = putBookAsync(bookId, obj);
            t.GetAwaiter().GetResult();
        }
        public async Task putBookAsync(ISBN isbn, Book obj)
        {
            var client = new ServantClient();
#if DEBUG
            var jsonObj = JsonConvert.SerializeObject(obj, Formatting.Indented);
#else
            var jsonObj = JsonConvert.SerializeObject(obj);
#endif
            var res = await client.PutAsync($"{server}/book/isbn/{isbn}", new StringContent(jsonObj, Encoding.UTF8, "application/json"));
            Debug.WriteLine($">>> {res.RequestMessage}");
            Debug.WriteLine($"-----\n{jsonObj}\n-----");
            Debug.WriteLine($"<<< {(int)res.StatusCode} {res.ReasonPhrase}");
            var content = await res.Content.ReadAsStringAsync();
            Debug.WriteLine($"<<< {content}");
            JsonConvert.DeserializeObject(content);
        }
        public void putBook(ISBN isbn, Book obj)
        {
            Task t = putBookAsync(isbn, obj);
            t.GetAwaiter().GetResult();
        }
        public async Task deleteBookAsync(BookId bookId)
        {
            var client = new ServantClient();
            var res = await client.DeleteAsync($"{server}/book/{bookId}");
            Debug.WriteLine($">>> {res.RequestMessage}");
            Debug.WriteLine($"<<< {(int)res.StatusCode} {res.ReasonPhrase}");
            var content = await res.Content.ReadAsStringAsync();
            Debug.WriteLine($"<<< {content}");
            JsonConvert.DeserializeObject(content);
        }
        public void deleteBook(BookId bookId)
        {
            Task t = deleteBookAsync(bookId);
            t.GetAwaiter().GetResult();
        }
        public async Task deleteBookAsync(ISBN isbn)
        {
            var client = new ServantClient();
            var res = await client.DeleteAsync($"{server}/book/isbn/{isbn}");
            Debug.WriteLine($">>> {res.RequestMessage}");
            Debug.WriteLine($"<<< {(int)res.StatusCode} {res.ReasonPhrase}");
            var content = await res.Content.ReadAsStringAsync();
            Debug.WriteLine($"<<< {content}");
            JsonConvert.DeserializeObject(content);
        }
        public void deleteBook(ISBN isbn)
        {
            Task t = deleteBookAsync(isbn);
            t.GetAwaiter().GetResult();
        }
        #endregion
    }
}
