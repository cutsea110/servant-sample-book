using Newtonsoft.Json;
using System.Collections.Generic;
using System.Net.Http;
using System.Net.Http.Headers;
using System.Text;
using System.Threading.Tasks;

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
        #region Address API
        public async Task<Address> getAddressAsync(int addressId)
        {
            var client = new ServantClient();
            var res = await client.GetAsync($"{server}/address/{addressId}");
            var content = await res.Content.ReadAsStringAsync();
            return JsonConvert.DeserializeObject<Address>(content);
        }
        public Address getAddress(int addressId)
        {
            Task<Address> t = getAddressAsync(addressId);
            return t.GetAwaiter().GetResult();
        }
        public async Task<List<Address>> getAddressesAsync()
        {
            var client = new ServantClient();
            var res = await client.GetAsync($"{server}/addresses");
            var content = await res.Content.ReadAsStringAsync();
            return JsonConvert.DeserializeObject<List<Address>>(content);
        }
        public List<Address> getAddresses()
        {
            Task<List<Address>> t = getAddressesAsync();
            return t.GetAwaiter().GetResult();
        }
        public async Task<int> postAddressAsync(Address obj)
        {
            var client = new ServantClient();
            var jsonObj = JsonConvert.SerializeObject(obj);
            var res = await client.PostAsync($"{server}/address", new StringContent(jsonObj, Encoding.UTF8, "application/json"));
            var content = await res.Content.ReadAsStringAsync();
            return JsonConvert.DeserializeObject<int>(content);
        }
        public int postAddress(Address obj)
        {
            Task<int> t = postAddressAsync(obj);
            return t.GetAwaiter().GetResult();
        }
        public async Task putAddressAsync(int addressId, Address obj)
        {
            var client = new ServantClient();
            var jsonObj = JsonConvert.SerializeObject(obj);
            var res = await client.PutAsync($"{server}/address/{addressId}", new StringContent(jsonObj, Encoding.UTF8, "application/json"));
            var content = await res.Content.ReadAsStringAsync();
            JsonConvert.DeserializeObject(content);
        }
        public void putAddress(int addressid, Address obj)
        {
            Task t = putAddressAsync(addressid, obj);
            t.GetAwaiter().GetResult();
        }
        public async Task deleteAddressAsync(int addressId)
        {
            var client = new ServantClient();
            var res = await client.DeleteAsync($"{server}/address/{addressId}");
            var content = await res.Content.ReadAsStringAsync();
            JsonConvert.DeserializeObject(content);
        }
        public void deleteAddress(int addressId)
        {
            Task t = deleteAddressAsync(addressId);
            t.GetAwaiter().GetResult();
        }
        #endregion
        #region Author
        public async Task<Author> getAuthorAsync(int authorId)
        {
            var client = new ServantClient();
            var res = await client.GetAsync($"{server}/author/{authorId}");
            var content = await res.Content.ReadAsStringAsync();
            return JsonConvert.DeserializeObject<Author>(content);
        }
        public Author getAuthor(int authorId)
        {
            Task<Author> t = getAuthorAsync(authorId);
            return t.GetAwaiter().GetResult();
        }
        public async Task<List<Author>> getAuthorsAsync()
        {
            var client = new ServantClient();
            var res = await client.GetAsync($"{server}/authors");
            var content = await res.Content.ReadAsStringAsync();
            return JsonConvert.DeserializeObject<List<Author>>(content);
        }
        public List<Author> getAuthors()
        {
            Task<List<Author>> t = getAuthorsAsync();
            return t.GetAwaiter().GetResult();
        }
        public async Task<int> postAuthorAsync(Author obj)
        {
            var client = new ServantClient();
            var jsonObj = JsonConvert.SerializeObject(obj);
            var res = await client.PostAsync($"{server}/author", new StringContent(jsonObj, Encoding.UTF8, "application/json"));
            var content = await res.Content.ReadAsStringAsync();
            return JsonConvert.DeserializeObject<int>(content);
        }
        public int postAuthor(Author obj)
        {
            Task<int> t = postAuthorAsync(obj);
            return t.GetAwaiter().GetResult();
        }
        public async Task putAuthorAsync(int authorId, Author obj)
        {
            var client = new ServantClient();
            var jsonObj = JsonConvert.SerializeObject(obj);
            var res = await client.PutAsync($"{server}/author/{authorId}", new StringContent(jsonObj, Encoding.UTF8, "application/json"));
            var content = await res.Content.ReadAsStringAsync();
            JsonConvert.DeserializeObject(content);
        }
        public void putAuthor(int authorId, Author obj)
        {
            Task t = putAuthorAsync(authorId, obj);
            t.GetAwaiter().GetResult();
        }
        public async Task deleteAuthorAsync(int authorId)
        {
            var client = new ServantClient();
            var res = await client.DeleteAsync($"{server}/author/{authorId}");
            var content = await res.Content.ReadAsStringAsync();
            JsonConvert.DeserializeObject(content);
        }
        public void deleteAuthor(int authorId)
        {
            Task t = deleteAuthorAsync(authorId);
            t.GetAwaiter().GetResult();
        }
        #endregion
        #region Publisher
        public async Task<Publisher> getPublisherAsync(int publisherId)
        {
            var client = new ServantClient();
            var res = await client.GetAsync($"{server}/publisher/{publisherId}");
            var content = await res.Content.ReadAsStringAsync();
            return JsonConvert.DeserializeObject<Publisher>(content);
        }
        public Publisher getPublisher(int publisherId)
        {
            Task<Publisher> t = getPublisherAsync(publisherId);
            return t.GetAwaiter().GetResult();
        }
        public async Task<List<Publisher>> getPublishersAsync()
        {
            var client = new ServantClient();
            var res = await client.GetAsync($"{server}/publishers");
            var content = await res.Content.ReadAsStringAsync();
            return JsonConvert.DeserializeObject<List<Publisher>>(content);
        }
        public List<Publisher> getPublishers()
        {
            Task<List<Publisher>> t = getPublishersAsync();
            return t.GetAwaiter().GetResult();
        }
        public async Task<int> postPublisherAsync(Publisher obj)
        {
            var client = new ServantClient();
            var jsonObj = JsonConvert.SerializeObject(obj);
            var res = await client.PostAsync($"{server}/publisher", new StringContent(jsonObj, Encoding.UTF8, "application/json"));
            var content = await res.Content.ReadAsStringAsync();
            return JsonConvert.DeserializeObject<int>(content);
        }
        public int postPublisher(Publisher obj)
        {
            Task<int> t = postPublisherAsync(obj);
            return t.GetAwaiter().GetResult();
        }
        public async Task putPublisherAsync(int publisherId, Publisher obj)
        {
            var client = new ServantClient();
            var jsonObj = JsonConvert.SerializeObject(obj);
            var res = await client.PutAsync($"{server}/publisher/{publisherId}", new StringContent(jsonObj, Encoding.UTF8, "application/json"));
            var content = await res.Content.ReadAsStringAsync();
            JsonConvert.DeserializeObject(content);
        }
        public void putPublisher(int publisherId, Publisher obj)
        {
            Task t = putPublisherAsync(publisherId, obj);
            t.GetAwaiter().GetResult();
        }
        public async Task deletePublisherAsync(int publisherId)
        {
            var client = new ServantClient();
            var res = await client.DeleteAsync($"{server}/publisher/{publisherId}");
            var content = await res.Content.ReadAsStringAsync();
            JsonConvert.DeserializeObject(content);
        }
        public void deletePublisher(int publisherId)
        {
            Task t = deletePublisherAsync(publisherId);
            t.GetAwaiter().GetResult();
        }
        #endregion
        #region Book
        public async Task<Book> getBookAsync(int bookId)
        {
            var client = new ServantClient();
            var res = await client.GetAsync($"{server}/book/{bookId}");
            var content = await res.Content.ReadAsStringAsync();
            return JsonConvert.DeserializeObject<Book>(content);
        }
        public Book getBook(int bookId)
        {
            Task<Book> t = getBookAsync(bookId);
            return t.GetAwaiter().GetResult();
        }
        public async Task<Book> getBookAsync(string isbn)
        {
            var client = new ServantClient();
            var res = await client.GetAsync($"{server}/book/isbn/{isbn}");
            var content = await res.Content.ReadAsStringAsync();
            return JsonConvert.DeserializeObject<Book>(content);
        }
        public Book getBook(string isbn)
        {
            Task<Book> t = getBookAsync(isbn);
            return t.GetAwaiter().GetResult();
        }
        public async Task<List<Book>> getBooksAsync()
        {
            var client = new ServantClient();
            var res = await client.GetAsync($"{server}/books");
            var content = await res.Content.ReadAsStringAsync();
            return JsonConvert.DeserializeObject<List<Book>>(content);
        }
        public List<Book> getBooks()
        {
            Task<List<Book>> t = getBooksAsync();
            return t.GetAwaiter().GetResult();
        }
        public async Task<int> postBookAsync(Book obj)
        {
            var client = new ServantClient();
            var jsonObj = JsonConvert.SerializeObject(obj);
            var res = await client.PostAsync($"{server}/book", new StringContent(jsonObj, Encoding.UTF8, "application/json"));
            var content = await res.Content.ReadAsStringAsync();
            return JsonConvert.DeserializeObject<int>(content);
        }
        public int postBook(Book obj)
        {
            Task<int> t = postBookAsync(obj);
            return t.GetAwaiter().GetResult();
        }
        public async Task putBookAsync(int bookId, Book obj)
        {
            var client = new ServantClient();
            var jsonObj = JsonConvert.SerializeObject(obj);
            var res = await client.PutAsync($"{server}/book/{bookId}", new StringContent(jsonObj, Encoding.UTF8, "application/json"));
            var content = await res.Content.ReadAsStringAsync();
            JsonConvert.DeserializeObject(content);
        }
        public void putBook(int bookId, Book obj)
        {
            Task t = putBookAsync(bookId, obj);
            t.GetAwaiter().GetResult();
        }
        public async Task putBookAsync(string isbn, Book obj)
        {
            var client = new ServantClient();
            var jsonObj = JsonConvert.SerializeObject(obj);
            var res = await client.PutAsync($"{server}/book/isbn/{isbn}", new StringContent(jsonObj, Encoding.UTF8, "application/json"));
            var content = await res.Content.ReadAsStringAsync();
            JsonConvert.DeserializeObject(content);
        }
        public void putBook(string isbn, Book obj)
        {
            Task t = putBookAsync(isbn, obj);
            t.GetAwaiter().GetResult();
        }
        public async Task deleteBookAsync(int bookId)
        {
            var client = new ServantClient();
            var res = await client.DeleteAsync($"{server}/book/{bookId}");
            var content = await res.Content.ReadAsStringAsync();
            JsonConvert.DeserializeObject(content);
        }
        public void deleteBook(int bookId)
        {
            Task t = deleteBookAsync(bookId);
            t.GetAwaiter().GetResult();
        }
        public async Task deleteBookAsync(string isbn)
        {
            var client = new ServantClient();
            var res = await client.DeleteAsync($"{server}/book/isbn/{isbn}");
            var content = await res.Content.ReadAsStringAsync();
            JsonConvert.DeserializeObject(content);
        }
        public void deleteBook(string isbn)
        {
            Task t = deleteBookAsync(isbn);
            t.GetAwaiter().GetResult();
        }
        #endregion
    }
}
