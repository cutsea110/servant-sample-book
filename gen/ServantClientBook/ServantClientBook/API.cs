
using Newtonsoft.Json;
using System.Collections.Generic;
using System.Diagnostics;
using System.Linq;
using System.Net.Http;
using System.Net.Http.Headers;
using System.Text;
using System.Threading.Tasks;

#region type alias
using Postcode = System.String;
using Tel = System.String;
using Fax = System.String;
using Emailaddress = System.String;
using ISBN = System.String;
using AddressId = System.Int64;
using AuthorId = System.Int64;
using PublisherId = System.Int64;
using BookId = System.Int64;
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
        #region fields
        private string server;
        #endregion

        #region properties
        #endregion

        #region Constructor
        public API(string _server)
        {
            this.server = _server;
        }
        #endregion

        #region APIs
         public async Task<AddressList> getAddressesAsync(int? _page = null, int? _per_page = null)
         {
             var client = new ServantClient();
             var queryparams = new List<string> {
                 _page.HasValue ? $"_page={_page.Value}" : null,
                 _per_page.HasValue ? $"_per_page={_per_page.Value}" : null,
             }.Where(e => !string.IsNullOrEmpty(e));
             var qp= queryparams.Count() > 0 ? $"?{string.Join("&", queryparams)}" : "";
             var res = await client.GetAsync($"{server}/addresses{qp}");
             Debug.WriteLine($">>> {res.RequestMessage}");
             Debug.WriteLine($"<<< {(int)res.StatusCode} {res.ReasonPhrase}");
             var content = await res.Content.ReadAsStringAsync();
             Debug.WriteLine($"<<< {content}");
             return JsonConvert.DeserializeObject<AddressList>(content);
        }
         public AddressList getAddresses(int? _page = null, int? _per_page = null)
         {
             Task<AddressList> t = getAddressesAsync(_page, _per_page);
             return t.GetAwaiter().GetResult();
         }
         public async Task<AddressList> postAddressesAsync(AddressQueryCondition _obj, int? _page = null, int? _per_page = null)
         {
             var client = new ServantClient();
             var queryparams = new List<string> {
                 _page.HasValue ? $"_page={_page.Value}" : null,
                 _per_page.HasValue ? $"_per_page={_per_page.Value}" : null,
             }.Where(e => !string.IsNullOrEmpty(e));
             var qp= queryparams.Count() > 0 ? $"?{string.Join("&", queryparams)}" : "";
             #if DEBUG
             var jsonObj = JsonConvert.SerializeObject(_obj, Formatting.Indented);
             #else
             var jsonObj = JsonConvert.SerializeObject(_obj);
             #endif
             var res = await client.PostAsync($"{server}/addresses{qp}", new StringContent(jsonObj, Encoding.UTF8, "application/json"));
             Debug.WriteLine($">>> {res.RequestMessage}");
             Debug.WriteLine($"-----");
             Debug.WriteLine(jsonObj);
             Debug.WriteLine($"-----");
             Debug.WriteLine($"<<< {(int)res.StatusCode} {res.ReasonPhrase}");
             var content = await res.Content.ReadAsStringAsync();
             Debug.WriteLine($"<<< {content}");
             return JsonConvert.DeserializeObject<AddressList>(content);
        }
         public AddressList postAddresses(AddressQueryCondition _obj, int? _page = null, int? _per_page = null)
         {
             Task<AddressList> t = postAddressesAsync(_obj, _page, _per_page);
             return t.GetAwaiter().GetResult();
         }
         public async Task<AddressId> postAddressAsync(Address _obj)
         {
             var client = new ServantClient();
             var queryparams = new List<string> {
             }.Where(e => !string.IsNullOrEmpty(e));
             var qp= queryparams.Count() > 0 ? $"?{string.Join("&", queryparams)}" : "";
             #if DEBUG
             var jsonObj = JsonConvert.SerializeObject(_obj, Formatting.Indented);
             #else
             var jsonObj = JsonConvert.SerializeObject(_obj);
             #endif
             var res = await client.PostAsync($"{server}/address{qp}", new StringContent(jsonObj, Encoding.UTF8, "application/json"));
             Debug.WriteLine($">>> {res.RequestMessage}");
             Debug.WriteLine($"-----");
             Debug.WriteLine(jsonObj);
             Debug.WriteLine($"-----");
             Debug.WriteLine($"<<< {(int)res.StatusCode} {res.ReasonPhrase}");
             var content = await res.Content.ReadAsStringAsync();
             Debug.WriteLine($"<<< {content}");
             return JsonConvert.DeserializeObject<AddressId>(content);
        }
         public AddressId postAddress(Address _obj)
         {
             Task<AddressId> t = postAddressAsync(_obj);
             return t.GetAwaiter().GetResult();
         }
         public async Task<Address> getAddressByIdAsync(AddressId _id)
         {
             var client = new ServantClient();
             var queryparams = new List<string> {
             }.Where(e => !string.IsNullOrEmpty(e));
             var qp= queryparams.Count() > 0 ? $"?{string.Join("&", queryparams)}" : "";
             var res = await client.GetAsync($"{server}/address/{_id}{qp}");
             Debug.WriteLine($">>> {res.RequestMessage}");
             Debug.WriteLine($"<<< {(int)res.StatusCode} {res.ReasonPhrase}");
             var content = await res.Content.ReadAsStringAsync();
             Debug.WriteLine($"<<< {content}");
             return JsonConvert.DeserializeObject<Address>(content);
        }
         public Address getAddressById(AddressId _id)
         {
             Task<Address> t = getAddressByIdAsync(_id);
             return t.GetAwaiter().GetResult();
         }
         public async Task putAddressByIdAsync(AddressId _id, Address _obj)
         {
             var client = new ServantClient();
             var queryparams = new List<string> {
             }.Where(e => !string.IsNullOrEmpty(e));
             var qp= queryparams.Count() > 0 ? $"?{string.Join("&", queryparams)}" : "";
             #if DEBUG
             var jsonObj = JsonConvert.SerializeObject(_obj, Formatting.Indented);
             #else
             var jsonObj = JsonConvert.SerializeObject(_obj);
             #endif
             var res = await client.PutAsync($"{server}/address/{_id}{qp}", new StringContent(jsonObj, Encoding.UTF8, "application/json"));
             Debug.WriteLine($">>> {res.RequestMessage}");
             Debug.WriteLine($"-----");
             Debug.WriteLine(jsonObj);
             Debug.WriteLine($"-----");
             Debug.WriteLine($"<<< {(int)res.StatusCode} {res.ReasonPhrase}");
             var content = await res.Content.ReadAsStringAsync();
             Debug.WriteLine($"<<< {content}");
             JsonConvert.DeserializeObject(content);
        }
         public void putAddressById(AddressId _id, Address _obj)
         {
             Task t = putAddressByIdAsync(_id, _obj);
             t.GetAwaiter().GetResult();
         }
         public async Task deleteAddressByIdAsync(AddressId _id)
         {
             var client = new ServantClient();
             var queryparams = new List<string> {
             }.Where(e => !string.IsNullOrEmpty(e));
             var qp= queryparams.Count() > 0 ? $"?{string.Join("&", queryparams)}" : "";
             var res = await client.DeleteAsync($"{server}/address/{_id}{qp}");
             Debug.WriteLine($">>> {res.RequestMessage}");
             Debug.WriteLine($"<<< {(int)res.StatusCode} {res.ReasonPhrase}");
             var content = await res.Content.ReadAsStringAsync();
             Debug.WriteLine($"<<< {content}");
             JsonConvert.DeserializeObject(content);
        }
         public void deleteAddressById(AddressId _id)
         {
             Task t = deleteAddressByIdAsync(_id);
             t.GetAwaiter().GetResult();
         }
         public async Task<AuthorList> getAuthorsAsync(int? _page = null, int? _per_page = null)
         {
             var client = new ServantClient();
             var queryparams = new List<string> {
                 _page.HasValue ? $"_page={_page.Value}" : null,
                 _per_page.HasValue ? $"_per_page={_per_page.Value}" : null,
             }.Where(e => !string.IsNullOrEmpty(e));
             var qp= queryparams.Count() > 0 ? $"?{string.Join("&", queryparams)}" : "";
             var res = await client.GetAsync($"{server}/authors{qp}");
             Debug.WriteLine($">>> {res.RequestMessage}");
             Debug.WriteLine($"<<< {(int)res.StatusCode} {res.ReasonPhrase}");
             var content = await res.Content.ReadAsStringAsync();
             Debug.WriteLine($"<<< {content}");
             return JsonConvert.DeserializeObject<AuthorList>(content);
        }
         public AuthorList getAuthors(int? _page = null, int? _per_page = null)
         {
             Task<AuthorList> t = getAuthorsAsync(_page, _per_page);
             return t.GetAwaiter().GetResult();
         }
         public async Task<AuthorList> postAuthorsAsync(AuthorQueryCondition _obj, int? _page = null, int? _per_page = null)
         {
             var client = new ServantClient();
             var queryparams = new List<string> {
                 _page.HasValue ? $"_page={_page.Value}" : null,
                 _per_page.HasValue ? $"_per_page={_per_page.Value}" : null,
             }.Where(e => !string.IsNullOrEmpty(e));
             var qp= queryparams.Count() > 0 ? $"?{string.Join("&", queryparams)}" : "";
             #if DEBUG
             var jsonObj = JsonConvert.SerializeObject(_obj, Formatting.Indented);
             #else
             var jsonObj = JsonConvert.SerializeObject(_obj);
             #endif
             var res = await client.PostAsync($"{server}/authors{qp}", new StringContent(jsonObj, Encoding.UTF8, "application/json"));
             Debug.WriteLine($">>> {res.RequestMessage}");
             Debug.WriteLine($"-----");
             Debug.WriteLine(jsonObj);
             Debug.WriteLine($"-----");
             Debug.WriteLine($"<<< {(int)res.StatusCode} {res.ReasonPhrase}");
             var content = await res.Content.ReadAsStringAsync();
             Debug.WriteLine($"<<< {content}");
             return JsonConvert.DeserializeObject<AuthorList>(content);
        }
         public AuthorList postAuthors(AuthorQueryCondition _obj, int? _page = null, int? _per_page = null)
         {
             Task<AuthorList> t = postAuthorsAsync(_obj, _page, _per_page);
             return t.GetAwaiter().GetResult();
         }
         public async Task<AuthorId> postAuthorAsync(Author _obj)
         {
             var client = new ServantClient();
             var queryparams = new List<string> {
             }.Where(e => !string.IsNullOrEmpty(e));
             var qp= queryparams.Count() > 0 ? $"?{string.Join("&", queryparams)}" : "";
             #if DEBUG
             var jsonObj = JsonConvert.SerializeObject(_obj, Formatting.Indented);
             #else
             var jsonObj = JsonConvert.SerializeObject(_obj);
             #endif
             var res = await client.PostAsync($"{server}/author{qp}", new StringContent(jsonObj, Encoding.UTF8, "application/json"));
             Debug.WriteLine($">>> {res.RequestMessage}");
             Debug.WriteLine($"-----");
             Debug.WriteLine(jsonObj);
             Debug.WriteLine($"-----");
             Debug.WriteLine($"<<< {(int)res.StatusCode} {res.ReasonPhrase}");
             var content = await res.Content.ReadAsStringAsync();
             Debug.WriteLine($"<<< {content}");
             return JsonConvert.DeserializeObject<AuthorId>(content);
        }
         public AuthorId postAuthor(Author _obj)
         {
             Task<AuthorId> t = postAuthorAsync(_obj);
             return t.GetAwaiter().GetResult();
         }
         public async Task<Author> getAuthorByIdAsync(AuthorId _id)
         {
             var client = new ServantClient();
             var queryparams = new List<string> {
             }.Where(e => !string.IsNullOrEmpty(e));
             var qp= queryparams.Count() > 0 ? $"?{string.Join("&", queryparams)}" : "";
             var res = await client.GetAsync($"{server}/author/{_id}{qp}");
             Debug.WriteLine($">>> {res.RequestMessage}");
             Debug.WriteLine($"<<< {(int)res.StatusCode} {res.ReasonPhrase}");
             var content = await res.Content.ReadAsStringAsync();
             Debug.WriteLine($"<<< {content}");
             return JsonConvert.DeserializeObject<Author>(content);
        }
         public Author getAuthorById(AuthorId _id)
         {
             Task<Author> t = getAuthorByIdAsync(_id);
             return t.GetAwaiter().GetResult();
         }
         public async Task putAuthorByIdAsync(AuthorId _id, Author _obj)
         {
             var client = new ServantClient();
             var queryparams = new List<string> {
             }.Where(e => !string.IsNullOrEmpty(e));
             var qp= queryparams.Count() > 0 ? $"?{string.Join("&", queryparams)}" : "";
             #if DEBUG
             var jsonObj = JsonConvert.SerializeObject(_obj, Formatting.Indented);
             #else
             var jsonObj = JsonConvert.SerializeObject(_obj);
             #endif
             var res = await client.PutAsync($"{server}/author/{_id}{qp}", new StringContent(jsonObj, Encoding.UTF8, "application/json"));
             Debug.WriteLine($">>> {res.RequestMessage}");
             Debug.WriteLine($"-----");
             Debug.WriteLine(jsonObj);
             Debug.WriteLine($"-----");
             Debug.WriteLine($"<<< {(int)res.StatusCode} {res.ReasonPhrase}");
             var content = await res.Content.ReadAsStringAsync();
             Debug.WriteLine($"<<< {content}");
             JsonConvert.DeserializeObject(content);
        }
         public void putAuthorById(AuthorId _id, Author _obj)
         {
             Task t = putAuthorByIdAsync(_id, _obj);
             t.GetAwaiter().GetResult();
         }
         public async Task deleteAuthorByIdAsync(AuthorId _id)
         {
             var client = new ServantClient();
             var queryparams = new List<string> {
             }.Where(e => !string.IsNullOrEmpty(e));
             var qp= queryparams.Count() > 0 ? $"?{string.Join("&", queryparams)}" : "";
             var res = await client.DeleteAsync($"{server}/author/{_id}{qp}");
             Debug.WriteLine($">>> {res.RequestMessage}");
             Debug.WriteLine($"<<< {(int)res.StatusCode} {res.ReasonPhrase}");
             var content = await res.Content.ReadAsStringAsync();
             Debug.WriteLine($"<<< {content}");
             JsonConvert.DeserializeObject(content);
        }
         public void deleteAuthorById(AuthorId _id)
         {
             Task t = deleteAuthorByIdAsync(_id);
             t.GetAwaiter().GetResult();
         }
         public async Task<PublisherList> getPublishersAsync(int? _page = null, int? _per_page = null)
         {
             var client = new ServantClient();
             var queryparams = new List<string> {
                 _page.HasValue ? $"_page={_page.Value}" : null,
                 _per_page.HasValue ? $"_per_page={_per_page.Value}" : null,
             }.Where(e => !string.IsNullOrEmpty(e));
             var qp= queryparams.Count() > 0 ? $"?{string.Join("&", queryparams)}" : "";
             var res = await client.GetAsync($"{server}/publishers{qp}");
             Debug.WriteLine($">>> {res.RequestMessage}");
             Debug.WriteLine($"<<< {(int)res.StatusCode} {res.ReasonPhrase}");
             var content = await res.Content.ReadAsStringAsync();
             Debug.WriteLine($"<<< {content}");
             return JsonConvert.DeserializeObject<PublisherList>(content);
        }
         public PublisherList getPublishers(int? _page = null, int? _per_page = null)
         {
             Task<PublisherList> t = getPublishersAsync(_page, _per_page);
             return t.GetAwaiter().GetResult();
         }
         public async Task<PublisherList> postPublishersAsync(PublisherQueryCondition _obj, int? _page = null, int? _per_page = null)
         {
             var client = new ServantClient();
             var queryparams = new List<string> {
                 _page.HasValue ? $"_page={_page.Value}" : null,
                 _per_page.HasValue ? $"_per_page={_per_page.Value}" : null,
             }.Where(e => !string.IsNullOrEmpty(e));
             var qp= queryparams.Count() > 0 ? $"?{string.Join("&", queryparams)}" : "";
             #if DEBUG
             var jsonObj = JsonConvert.SerializeObject(_obj, Formatting.Indented);
             #else
             var jsonObj = JsonConvert.SerializeObject(_obj);
             #endif
             var res = await client.PostAsync($"{server}/publishers{qp}", new StringContent(jsonObj, Encoding.UTF8, "application/json"));
             Debug.WriteLine($">>> {res.RequestMessage}");
             Debug.WriteLine($"-----");
             Debug.WriteLine(jsonObj);
             Debug.WriteLine($"-----");
             Debug.WriteLine($"<<< {(int)res.StatusCode} {res.ReasonPhrase}");
             var content = await res.Content.ReadAsStringAsync();
             Debug.WriteLine($"<<< {content}");
             return JsonConvert.DeserializeObject<PublisherList>(content);
        }
         public PublisherList postPublishers(PublisherQueryCondition _obj, int? _page = null, int? _per_page = null)
         {
             Task<PublisherList> t = postPublishersAsync(_obj, _page, _per_page);
             return t.GetAwaiter().GetResult();
         }
         public async Task<PublisherId> postPublisherAsync(Publisher _obj)
         {
             var client = new ServantClient();
             var queryparams = new List<string> {
             }.Where(e => !string.IsNullOrEmpty(e));
             var qp= queryparams.Count() > 0 ? $"?{string.Join("&", queryparams)}" : "";
             #if DEBUG
             var jsonObj = JsonConvert.SerializeObject(_obj, Formatting.Indented);
             #else
             var jsonObj = JsonConvert.SerializeObject(_obj);
             #endif
             var res = await client.PostAsync($"{server}/publisher{qp}", new StringContent(jsonObj, Encoding.UTF8, "application/json"));
             Debug.WriteLine($">>> {res.RequestMessage}");
             Debug.WriteLine($"-----");
             Debug.WriteLine(jsonObj);
             Debug.WriteLine($"-----");
             Debug.WriteLine($"<<< {(int)res.StatusCode} {res.ReasonPhrase}");
             var content = await res.Content.ReadAsStringAsync();
             Debug.WriteLine($"<<< {content}");
             return JsonConvert.DeserializeObject<PublisherId>(content);
        }
         public PublisherId postPublisher(Publisher _obj)
         {
             Task<PublisherId> t = postPublisherAsync(_obj);
             return t.GetAwaiter().GetResult();
         }
         public async Task<Publisher> getPublisherByIdAsync(PublisherId _id)
         {
             var client = new ServantClient();
             var queryparams = new List<string> {
             }.Where(e => !string.IsNullOrEmpty(e));
             var qp= queryparams.Count() > 0 ? $"?{string.Join("&", queryparams)}" : "";
             var res = await client.GetAsync($"{server}/publisher/{_id}{qp}");
             Debug.WriteLine($">>> {res.RequestMessage}");
             Debug.WriteLine($"<<< {(int)res.StatusCode} {res.ReasonPhrase}");
             var content = await res.Content.ReadAsStringAsync();
             Debug.WriteLine($"<<< {content}");
             return JsonConvert.DeserializeObject<Publisher>(content);
        }
         public Publisher getPublisherById(PublisherId _id)
         {
             Task<Publisher> t = getPublisherByIdAsync(_id);
             return t.GetAwaiter().GetResult();
         }
         public async Task putPublisherByIdAsync(PublisherId _id, Publisher _obj)
         {
             var client = new ServantClient();
             var queryparams = new List<string> {
             }.Where(e => !string.IsNullOrEmpty(e));
             var qp= queryparams.Count() > 0 ? $"?{string.Join("&", queryparams)}" : "";
             #if DEBUG
             var jsonObj = JsonConvert.SerializeObject(_obj, Formatting.Indented);
             #else
             var jsonObj = JsonConvert.SerializeObject(_obj);
             #endif
             var res = await client.PutAsync($"{server}/publisher/{_id}{qp}", new StringContent(jsonObj, Encoding.UTF8, "application/json"));
             Debug.WriteLine($">>> {res.RequestMessage}");
             Debug.WriteLine($"-----");
             Debug.WriteLine(jsonObj);
             Debug.WriteLine($"-----");
             Debug.WriteLine($"<<< {(int)res.StatusCode} {res.ReasonPhrase}");
             var content = await res.Content.ReadAsStringAsync();
             Debug.WriteLine($"<<< {content}");
             JsonConvert.DeserializeObject(content);
        }
         public void putPublisherById(PublisherId _id, Publisher _obj)
         {
             Task t = putPublisherByIdAsync(_id, _obj);
             t.GetAwaiter().GetResult();
         }
         public async Task deletePublisherByIdAsync(PublisherId _id)
         {
             var client = new ServantClient();
             var queryparams = new List<string> {
             }.Where(e => !string.IsNullOrEmpty(e));
             var qp= queryparams.Count() > 0 ? $"?{string.Join("&", queryparams)}" : "";
             var res = await client.DeleteAsync($"{server}/publisher/{_id}{qp}");
             Debug.WriteLine($">>> {res.RequestMessage}");
             Debug.WriteLine($"<<< {(int)res.StatusCode} {res.ReasonPhrase}");
             var content = await res.Content.ReadAsStringAsync();
             Debug.WriteLine($"<<< {content}");
             JsonConvert.DeserializeObject(content);
        }
         public void deletePublisherById(PublisherId _id)
         {
             Task t = deletePublisherByIdAsync(_id);
             t.GetAwaiter().GetResult();
         }
         public async Task<BookList> getBooksAsync(int? _page = null, int? _per_page = null)
         {
             var client = new ServantClient();
             var queryparams = new List<string> {
                 _page.HasValue ? $"_page={_page.Value}" : null,
                 _per_page.HasValue ? $"_per_page={_per_page.Value}" : null,
             }.Where(e => !string.IsNullOrEmpty(e));
             var qp= queryparams.Count() > 0 ? $"?{string.Join("&", queryparams)}" : "";
             var res = await client.GetAsync($"{server}/books{qp}");
             Debug.WriteLine($">>> {res.RequestMessage}");
             Debug.WriteLine($"<<< {(int)res.StatusCode} {res.ReasonPhrase}");
             var content = await res.Content.ReadAsStringAsync();
             Debug.WriteLine($"<<< {content}");
             return JsonConvert.DeserializeObject<BookList>(content);
        }
         public BookList getBooks(int? _page = null, int? _per_page = null)
         {
             Task<BookList> t = getBooksAsync(_page, _per_page);
             return t.GetAwaiter().GetResult();
         }
         public async Task<BookList> postBooksAsync(BookQueryCondition _obj, int? _page = null, int? _per_page = null)
         {
             var client = new ServantClient();
             var queryparams = new List<string> {
                 _page.HasValue ? $"_page={_page.Value}" : null,
                 _per_page.HasValue ? $"_per_page={_per_page.Value}" : null,
             }.Where(e => !string.IsNullOrEmpty(e));
             var qp= queryparams.Count() > 0 ? $"?{string.Join("&", queryparams)}" : "";
             #if DEBUG
             var jsonObj = JsonConvert.SerializeObject(_obj, Formatting.Indented);
             #else
             var jsonObj = JsonConvert.SerializeObject(_obj);
             #endif
             var res = await client.PostAsync($"{server}/books{qp}", new StringContent(jsonObj, Encoding.UTF8, "application/json"));
             Debug.WriteLine($">>> {res.RequestMessage}");
             Debug.WriteLine($"-----");
             Debug.WriteLine(jsonObj);
             Debug.WriteLine($"-----");
             Debug.WriteLine($"<<< {(int)res.StatusCode} {res.ReasonPhrase}");
             var content = await res.Content.ReadAsStringAsync();
             Debug.WriteLine($"<<< {content}");
             return JsonConvert.DeserializeObject<BookList>(content);
        }
         public BookList postBooks(BookQueryCondition _obj, int? _page = null, int? _per_page = null)
         {
             Task<BookList> t = postBooksAsync(_obj, _page, _per_page);
             return t.GetAwaiter().GetResult();
         }
         public async Task<BookId> postBookAsync(Book _obj)
         {
             var client = new ServantClient();
             var queryparams = new List<string> {
             }.Where(e => !string.IsNullOrEmpty(e));
             var qp= queryparams.Count() > 0 ? $"?{string.Join("&", queryparams)}" : "";
             #if DEBUG
             var jsonObj = JsonConvert.SerializeObject(_obj, Formatting.Indented);
             #else
             var jsonObj = JsonConvert.SerializeObject(_obj);
             #endif
             var res = await client.PostAsync($"{server}/book{qp}", new StringContent(jsonObj, Encoding.UTF8, "application/json"));
             Debug.WriteLine($">>> {res.RequestMessage}");
             Debug.WriteLine($"-----");
             Debug.WriteLine(jsonObj);
             Debug.WriteLine($"-----");
             Debug.WriteLine($"<<< {(int)res.StatusCode} {res.ReasonPhrase}");
             var content = await res.Content.ReadAsStringAsync();
             Debug.WriteLine($"<<< {content}");
             return JsonConvert.DeserializeObject<BookId>(content);
        }
         public BookId postBook(Book _obj)
         {
             Task<BookId> t = postBookAsync(_obj);
             return t.GetAwaiter().GetResult();
         }
         public async Task<Book> getBookByIdAsync(BookId _id)
         {
             var client = new ServantClient();
             var queryparams = new List<string> {
             }.Where(e => !string.IsNullOrEmpty(e));
             var qp= queryparams.Count() > 0 ? $"?{string.Join("&", queryparams)}" : "";
             var res = await client.GetAsync($"{server}/book/{_id}{qp}");
             Debug.WriteLine($">>> {res.RequestMessage}");
             Debug.WriteLine($"<<< {(int)res.StatusCode} {res.ReasonPhrase}");
             var content = await res.Content.ReadAsStringAsync();
             Debug.WriteLine($"<<< {content}");
             return JsonConvert.DeserializeObject<Book>(content);
        }
         public Book getBookById(BookId _id)
         {
             Task<Book> t = getBookByIdAsync(_id);
             return t.GetAwaiter().GetResult();
         }
         public async Task putBookByIdAsync(BookId _id, Book _obj)
         {
             var client = new ServantClient();
             var queryparams = new List<string> {
             }.Where(e => !string.IsNullOrEmpty(e));
             var qp= queryparams.Count() > 0 ? $"?{string.Join("&", queryparams)}" : "";
             #if DEBUG
             var jsonObj = JsonConvert.SerializeObject(_obj, Formatting.Indented);
             #else
             var jsonObj = JsonConvert.SerializeObject(_obj);
             #endif
             var res = await client.PutAsync($"{server}/book/{_id}{qp}", new StringContent(jsonObj, Encoding.UTF8, "application/json"));
             Debug.WriteLine($">>> {res.RequestMessage}");
             Debug.WriteLine($"-----");
             Debug.WriteLine(jsonObj);
             Debug.WriteLine($"-----");
             Debug.WriteLine($"<<< {(int)res.StatusCode} {res.ReasonPhrase}");
             var content = await res.Content.ReadAsStringAsync();
             Debug.WriteLine($"<<< {content}");
             JsonConvert.DeserializeObject(content);
        }
         public void putBookById(BookId _id, Book _obj)
         {
             Task t = putBookByIdAsync(_id, _obj);
             t.GetAwaiter().GetResult();
         }
         public async Task deleteBookByIdAsync(BookId _id)
         {
             var client = new ServantClient();
             var queryparams = new List<string> {
             }.Where(e => !string.IsNullOrEmpty(e));
             var qp= queryparams.Count() > 0 ? $"?{string.Join("&", queryparams)}" : "";
             var res = await client.DeleteAsync($"{server}/book/{_id}{qp}");
             Debug.WriteLine($">>> {res.RequestMessage}");
             Debug.WriteLine($"<<< {(int)res.StatusCode} {res.ReasonPhrase}");
             var content = await res.Content.ReadAsStringAsync();
             Debug.WriteLine($"<<< {content}");
             JsonConvert.DeserializeObject(content);
        }
         public void deleteBookById(BookId _id)
         {
             Task t = deleteBookByIdAsync(_id);
             t.GetAwaiter().GetResult();
         }
         public async Task<Book> getBookIsbnByIsbnAsync(ISBN _isbn)
         {
             var client = new ServantClient();
             var queryparams = new List<string> {
             }.Where(e => !string.IsNullOrEmpty(e));
             var qp= queryparams.Count() > 0 ? $"?{string.Join("&", queryparams)}" : "";
             var res = await client.GetAsync($"{server}/book/isbn/{_isbn}{qp}");
             Debug.WriteLine($">>> {res.RequestMessage}");
             Debug.WriteLine($"<<< {(int)res.StatusCode} {res.ReasonPhrase}");
             var content = await res.Content.ReadAsStringAsync();
             Debug.WriteLine($"<<< {content}");
             return JsonConvert.DeserializeObject<Book>(content);
        }
         public Book getBookIsbnByIsbn(ISBN _isbn)
         {
             Task<Book> t = getBookIsbnByIsbnAsync(_isbn);
             return t.GetAwaiter().GetResult();
         }
         public async Task putBookIsbnByIsbnAsync(ISBN _isbn, Book _obj)
         {
             var client = new ServantClient();
             var queryparams = new List<string> {
             }.Where(e => !string.IsNullOrEmpty(e));
             var qp= queryparams.Count() > 0 ? $"?{string.Join("&", queryparams)}" : "";
             #if DEBUG
             var jsonObj = JsonConvert.SerializeObject(_obj, Formatting.Indented);
             #else
             var jsonObj = JsonConvert.SerializeObject(_obj);
             #endif
             var res = await client.PutAsync($"{server}/book/isbn/{_isbn}{qp}", new StringContent(jsonObj, Encoding.UTF8, "application/json"));
             Debug.WriteLine($">>> {res.RequestMessage}");
             Debug.WriteLine($"-----");
             Debug.WriteLine(jsonObj);
             Debug.WriteLine($"-----");
             Debug.WriteLine($"<<< {(int)res.StatusCode} {res.ReasonPhrase}");
             var content = await res.Content.ReadAsStringAsync();
             Debug.WriteLine($"<<< {content}");
             JsonConvert.DeserializeObject(content);
        }
         public void putBookIsbnByIsbn(ISBN _isbn, Book _obj)
         {
             Task t = putBookIsbnByIsbnAsync(_isbn, _obj);
             t.GetAwaiter().GetResult();
         }
         public async Task deleteBookIsbnByIsbnAsync(ISBN _isbn)
         {
             var client = new ServantClient();
             var queryparams = new List<string> {
             }.Where(e => !string.IsNullOrEmpty(e));
             var qp= queryparams.Count() > 0 ? $"?{string.Join("&", queryparams)}" : "";
             var res = await client.DeleteAsync($"{server}/book/isbn/{_isbn}{qp}");
             Debug.WriteLine($">>> {res.RequestMessage}");
             Debug.WriteLine($"<<< {(int)res.StatusCode} {res.ReasonPhrase}");
             var content = await res.Content.ReadAsStringAsync();
             Debug.WriteLine($"<<< {content}");
             JsonConvert.DeserializeObject(content);
        }
         public void deleteBookIsbnByIsbn(ISBN _isbn)
         {
             Task t = deleteBookIsbnByIsbnAsync(_isbn);
             t.GetAwaiter().GetResult();
         }
        #endregion
    }
}
