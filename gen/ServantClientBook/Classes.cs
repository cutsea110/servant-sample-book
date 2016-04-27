
using Newtonsoft.Json;
using Newtonsoft.Json.Converters;
using System;
using System.Collections.Generic;

#region type alias
using Fax = System.String;
using AuthorId = System.Int64;
using AddressId = System.Int64;
using ISBN = System.String;
using BookId = System.Int64;
using Day = System.DateTime;
using UTCTime = System.DateTime;
using Emailaddress = System.String;
using Postcode = System.String;
using Tel = System.String;
using PublisherId = System.Int64;
#endregion

namespace ServantClientBook
{
    #region PublisherList
    [JsonObject("PublisherList")]
    public class PublisherList
    {
        [JsonProperty(PropertyName = "page")]
        public int page { get; set; }
        [JsonProperty(PropertyName = "result")]
        public List<Publisher> result { get; set; }
        [JsonProperty(PropertyName = "per_page")]
        public int per_page { get; set; }
        [JsonProperty(PropertyName = "hits")]
        public int hits { get; set; }
    }
    #endregion
    #region PublisherQueryCondition
    [JsonObject("PublisherQueryCondition")]
    public class PublisherQueryCondition
    {
        [JsonProperty(PropertyName = "publisherNameLike")]
        public string publisherNameLike { get; set; }
        [JsonProperty(PropertyName = "prefectureIn", ItemConverterType = typeof(StringEnumConverter))]
        public List<Prefecture> prefectureIn { get; set; }
        [JsonProperty(PropertyName = "companyTypeIn", ItemConverterType = typeof(StringEnumConverter))]
        public List<CompanyType> companyTypeIn { get; set; }
    }
    #endregion
    #region BookQueryCondition
    [JsonObject("BookQueryCondition")]
    public class BookQueryCondition
    {
        [JsonProperty(PropertyName = "publisherNameLike")]
        public string publisherNameLike { get; set; }
        [JsonProperty(PropertyName = "publishedTo")]
        [JsonConverter(typeof(DayConverter))]
        public Day? publishedTo { get; set; }
        [JsonProperty(PropertyName = "bookIdIn")]
        public List<BookId> bookIdIn { get; set; }
        [JsonProperty(PropertyName = "publishedFrom")]
        [JsonConverter(typeof(DayConverter))]
        public Day? publishedFrom { get; set; }
        [JsonProperty(PropertyName = "categoryIn", ItemConverterType = typeof(StringEnumConverter))]
        public List<Category> categoryIn { get; set; }
        [JsonProperty(PropertyName = "isbnEq")]
        public ISBN isbnEq { get; set; }
        [JsonProperty(PropertyName = "bookIdEq")]
        public BookId? bookIdEq { get; set; }
        [JsonProperty(PropertyName = "authorNameLike")]
        public string authorNameLike { get; set; }
    }
    #endregion
    #region PublisherInfo
    [JsonObject("PublisherInfo")]
    public class PublisherInfo
    {
        [JsonProperty(PropertyName = "name")]
        public string name { get; set; }
        [JsonProperty(PropertyName = "publisherId")]
        public PublisherId publisherId { get; set; }
    }
    #endregion
    #region BookList
    [JsonObject("BookList")]
    public class BookList
    {
        [JsonProperty(PropertyName = "page")]
        public int page { get; set; }
        [JsonProperty(PropertyName = "result")]
        public List<Book> result { get; set; }
        [JsonProperty(PropertyName = "per_page")]
        public int per_page { get; set; }
        [JsonProperty(PropertyName = "hits")]
        public int hits { get; set; }
    }
    #endregion
    #region Address
    [JsonObject("Address")]
    public class Address
    {
        [JsonProperty(PropertyName = "email")]
        public Emailaddress email { get; set; }
        [JsonProperty(PropertyName = "fax")]
        public Fax fax { get; set; }
        [JsonProperty(PropertyName = "createdAt")]
        public UTCTime createdAt { get; set; }
        [JsonProperty(PropertyName = "addressId")]
        public AddressId? addressId { get; set; }
        [JsonProperty(PropertyName = "building")]
        public string building { get; set; }
        [JsonProperty(PropertyName = "address")]
        public string address { get; set; }
        [JsonProperty(PropertyName = "prefecture")]
        [JsonConverter(typeof(StringEnumConverter))]
        public Prefecture prefecture { get; set; }
        [JsonProperty(PropertyName = "tel")]
        public Tel tel { get; set; }
        [JsonProperty(PropertyName = "postcode")]
        public Postcode postcode { get; set; }
        [JsonProperty(PropertyName = "updatedAt")]
        public UTCTime updatedAt { get; set; }
    }
    #endregion
    #region Book
    [JsonObject("Book")]
    public class Book
    {
        [JsonProperty(PropertyName = "createdAt")]
        public UTCTime createdAt { get; set; }
        [JsonProperty(PropertyName = "publishedAt")]
        [JsonConverter(typeof(DayConverter))]
        public Day publishedAt { get; set; }
        [JsonProperty(PropertyName = "category")]
        [JsonConverter(typeof(StringEnumConverter))]
        public Category category { get; set; }
        [JsonProperty(PropertyName = "publishedBy")]
        public PublisherInfo publishedBy { get; set; }
        [JsonProperty(PropertyName = "authors")]
        public List<AuthorInfo> authors { get; set; }
        [JsonProperty(PropertyName = "isbn")]
        public ISBN isbn { get; set; }
        [JsonProperty(PropertyName = "bookId")]
        public BookId? bookId { get; set; }
        [JsonProperty(PropertyName = "updatedAt")]
        public UTCTime updatedAt { get; set; }
        [JsonProperty(PropertyName = "title")]
        public string title { get; set; }
        [JsonProperty(PropertyName = "description")]
        public string description { get; set; }
    }
    #endregion
    #region AddressList
    [JsonObject("AddressList")]
    public class AddressList
    {
        [JsonProperty(PropertyName = "page")]
        public int page { get; set; }
        [JsonProperty(PropertyName = "result")]
        public List<Address> result { get; set; }
        [JsonProperty(PropertyName = "per_page")]
        public int per_page { get; set; }
        [JsonProperty(PropertyName = "hits")]
        public int hits { get; set; }
    }
    #endregion
    #region AuthorQueryCondition
    [JsonObject("AuthorQueryCondition")]
    public class AuthorQueryCondition
    {
        [JsonProperty(PropertyName = "ageTo")]
        public int? ageTo { get; set; }
        [JsonProperty(PropertyName = "prefectureIn", ItemConverterType = typeof(StringEnumConverter))]
        public List<Prefecture> prefectureIn { get; set; }
        [JsonProperty(PropertyName = "ageFrom")]
        public int? ageFrom { get; set; }
        [JsonProperty(PropertyName = "genderEq")]
        [JsonConverter(typeof(StringEnumConverter))]
        public Gender? genderEq { get; set; }
        [JsonProperty(PropertyName = "authorNameLike")]
        public string authorNameLike { get; set; }
    }
    #endregion
    #region AddressQueryCondition
    [JsonObject("AddressQueryCondition")]
    public class AddressQueryCondition
    {
        [JsonProperty(PropertyName = "faxLike")]
        public string faxLike { get; set; }
        [JsonProperty(PropertyName = "emailLike")]
        public string emailLike { get; set; }
        [JsonProperty(PropertyName = "telLike")]
        public string telLike { get; set; }
        [JsonProperty(PropertyName = "postCodeLike")]
        public string postCodeLike { get; set; }
    }
    #endregion
    #region AuthorList
    [JsonObject("AuthorList")]
    public class AuthorList
    {
        [JsonProperty(PropertyName = "page")]
        public int page { get; set; }
        [JsonProperty(PropertyName = "result")]
        public List<Author> result { get; set; }
        [JsonProperty(PropertyName = "per_page")]
        public int per_page { get; set; }
        [JsonProperty(PropertyName = "hits")]
        public int hits { get; set; }
    }
    #endregion
    #region Author
    [JsonObject("Author")]
    public class Author
    {
        [JsonProperty(PropertyName = "createdAt")]
        public UTCTime createdAt { get; set; }
        [JsonProperty(PropertyName = "birth")]
        [JsonConverter(typeof(DayConverter))]
        public Day birth { get; set; }
        [JsonProperty(PropertyName = "authorId")]
        public AuthorId? authorId { get; set; }
        [JsonProperty(PropertyName = "age")]
        public int age { get; set; }
        [JsonProperty(PropertyName = "address")]
        public Address address { get; set; }
        [JsonProperty(PropertyName = "gender")]
        [JsonConverter(typeof(StringEnumConverter))]
        public Gender gender { get; set; }
        [JsonProperty(PropertyName = "name")]
        public string name { get; set; }
        [JsonProperty(PropertyName = "updatedAt")]
        public UTCTime updatedAt { get; set; }
    }
    #endregion
    #region AuthorInfo
    [JsonObject("AuthorInfo")]
    public class AuthorInfo
    {
        [JsonProperty(PropertyName = "authorId")]
        public AuthorId authorId { get; set; }
        [JsonProperty(PropertyName = "name")]
        public string name { get; set; }
    }
    #endregion
    #region Publisher
    [JsonObject("Publisher")]
    public class Publisher
    {
        [JsonProperty(PropertyName = "companyType")]
        [JsonConverter(typeof(StringEnumConverter))]
        public CompanyType companyType { get; set; }
        [JsonProperty(PropertyName = "createdAt")]
        public UTCTime createdAt { get; set; }
        [JsonProperty(PropertyName = "address")]
        public Address address { get; set; }
        [JsonProperty(PropertyName = "name")]
        public string name { get; set; }
        [JsonProperty(PropertyName = "updatedAt")]
        public UTCTime updatedAt { get; set; }
        [JsonProperty(PropertyName = "publisherId")]
        public PublisherId? publisherId { get; set; }
    }
    #endregion
}
