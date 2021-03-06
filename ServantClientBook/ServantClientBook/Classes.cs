/* generated by servant-csharp */
using Newtonsoft.Json;
using Newtonsoft.Json.Converters;
using System;
using System.Collections.Generic;

#region type alias
using Postcode = System.String;
using Tel = System.String;
using Fax = System.String;
using Emailaddress = System.String;
using ISBN = System.String;
using AddressId = System.Int64;
using AuthorId = System.Int64;
using PublisherId = System.Int64;
using AuthorName = System.String;
using PublisherName = System.String;
using BookId = System.Int64;
#endregion

namespace ServantClientBook
{
    #region Address
    [JsonObject("Address")]
    public class Address
    {
        [JsonProperty(PropertyName = "addressId")]
        public Nullable<AddressId> addressId { get; set; }
        [JsonProperty(PropertyName = "postcode")]
        public Postcode postcode { get; set; }
        [JsonProperty(PropertyName = "prefecture")]
        [JsonConverter(typeof(StringEnumConverter))]
        public Prefecture prefecture { get; set; }
        [JsonProperty(PropertyName = "address")]
        public string address { get; set; }
        [JsonProperty(PropertyName = "building")]
        public string building { get; set; }
        [JsonProperty(PropertyName = "tel")]
        public Tel tel { get; set; }
        [JsonProperty(PropertyName = "fax")]
        public Fax fax { get; set; }
        [JsonProperty(PropertyName = "email")]
        public Emailaddress email { get; set; }
        [JsonProperty(PropertyName = "createdAt")]
        public DateTime createdAt { get; set; }
        [JsonProperty(PropertyName = "updatedAt")]
        public DateTime updatedAt { get; set; }
    }
    #endregion
    #region AddressQueryCondition
    [JsonObject("AddressQueryCondition")]
    public class AddressQueryCondition
    {
        [JsonProperty(PropertyName = "postCodeLike")]
        public string postCodeLike { get; set; }
        [JsonProperty(PropertyName = "telLike")]
        public string telLike { get; set; }
        [JsonProperty(PropertyName = "faxLike")]
        public string faxLike { get; set; }
        [JsonProperty(PropertyName = "emailLike")]
        public string emailLike { get; set; }
    }
    #endregion
    #region AddressList
    [JsonObject("AddressList")]
    public class AddressList
    {
        [JsonProperty(PropertyName = "hits")]
        public int hits { get; set; }
        [JsonProperty(PropertyName = "page")]
        public int page { get; set; }
        [JsonProperty(PropertyName = "per_page")]
        public int per_page { get; set; }
        [JsonProperty(PropertyName = "result")]
        public List<Address> result { get; set; }
    }
    #endregion
    #region Author
    [JsonObject("Author")]
    public class Author
    {
        [JsonProperty(PropertyName = "authorId")]
        public Nullable<AuthorId> authorId { get; set; }
        [JsonProperty(PropertyName = "name")]
        public string name { get; set; }
        [JsonProperty(PropertyName = "gender")]
        [JsonConverter(typeof(StringEnumConverter))]
        public Gender gender { get; set; }
        [JsonProperty(PropertyName = "birth")]
        [JsonConverter(typeof(DayConverter))]
        public DateTime birth { get; set; }
        [JsonProperty(PropertyName = "age")]
        public int age { get; set; }
        [JsonProperty(PropertyName = "address")]
        public Address address { get; set; }
        [JsonProperty(PropertyName = "createdAt")]
        public DateTime createdAt { get; set; }
        [JsonProperty(PropertyName = "updatedAt")]
        public DateTime updatedAt { get; set; }
    }
    #endregion
    #region AuthorQueryCondition
    [JsonObject("AuthorQueryCondition")]
    public class AuthorQueryCondition
    {
        [JsonProperty(PropertyName = "authorNameLike")]
        public string authorNameLike { get; set; }
        [JsonProperty(PropertyName = "genderEq")]
        [JsonConverter(typeof(StringEnumConverter))]
        public Gender? genderEq { get; set; }
        [JsonProperty(PropertyName = "ageFrom")]
        public int? ageFrom { get; set; }
        [JsonProperty(PropertyName = "ageTo")]
        public int? ageTo { get; set; }
        [JsonProperty(PropertyName = "prefectureIn", ItemConverterType = typeof(StringEnumConverter))]
        public List<Prefecture> prefectureIn { get; set; }
    }
    #endregion
    #region AuthorList
    [JsonObject("AuthorList")]
    public class AuthorList
    {
        [JsonProperty(PropertyName = "hits")]
        public int hits { get; set; }
        [JsonProperty(PropertyName = "page")]
        public int page { get; set; }
        [JsonProperty(PropertyName = "per_page")]
        public int per_page { get; set; }
        [JsonProperty(PropertyName = "result")]
        public List<Author> result { get; set; }
    }
    #endregion
    #region Publisher
    [JsonObject("Publisher")]
    public class Publisher
    {
        [JsonProperty(PropertyName = "publisherId")]
        public Nullable<PublisherId> publisherId { get; set; }
        [JsonProperty(PropertyName = "name")]
        public string name { get; set; }
        [JsonProperty(PropertyName = "companyType")]
        [JsonConverter(typeof(StringEnumConverter))]
        public CompanyType companyType { get; set; }
        [JsonProperty(PropertyName = "address")]
        public Address address { get; set; }
        [JsonProperty(PropertyName = "createdAt")]
        public DateTime createdAt { get; set; }
        [JsonProperty(PropertyName = "updatedAt")]
        public DateTime updatedAt { get; set; }
    }
    #endregion
    #region PublisherQueryCondition
    [JsonObject("PublisherQueryCondition")]
    public class PublisherQueryCondition
    {
        [JsonProperty(PropertyName = "publisherNameLike")]
        public string publisherNameLike { get; set; }
        [JsonProperty(PropertyName = "companyTypeIn", ItemConverterType = typeof(StringEnumConverter))]
        public List<CompanyType> companyTypeIn { get; set; }
        [JsonProperty(PropertyName = "prefectureIn", ItemConverterType = typeof(StringEnumConverter))]
        public List<Prefecture> prefectureIn { get; set; }
    }
    #endregion
    #region PublisherList
    [JsonObject("PublisherList")]
    public class PublisherList
    {
        [JsonProperty(PropertyName = "hits")]
        public int hits { get; set; }
        [JsonProperty(PropertyName = "page")]
        public int page { get; set; }
        [JsonProperty(PropertyName = "per_page")]
        public int per_page { get; set; }
        [JsonProperty(PropertyName = "result")]
        public List<Publisher> result { get; set; }
    }
    #endregion
    #region AuthorInfo
    [JsonObject("AuthorInfo")]
    public class AuthorInfo
    {
        [JsonProperty(PropertyName = "authorId")]
        public AuthorId authorId { get; set; }
        [JsonProperty(PropertyName = "name")]
        public AuthorName name { get; set; }
    }
    #endregion
    #region PublisherInfo
    [JsonObject("PublisherInfo")]
    public class PublisherInfo
    {
        [JsonProperty(PropertyName = "publisherId")]
        public PublisherId publisherId { get; set; }
        [JsonProperty(PropertyName = "name")]
        public PublisherName name { get; set; }
    }
    #endregion
    #region Book
    [JsonObject("Book")]
    public class Book
    {
        [JsonProperty(PropertyName = "bookId")]
        public Nullable<BookId> bookId { get; set; }
        [JsonProperty(PropertyName = "title")]
        public string title { get; set; }
        [JsonProperty(PropertyName = "isbn")]
        public ISBN isbn { get; set; }
        [JsonProperty(PropertyName = "category")]
        [JsonConverter(typeof(StringEnumConverter))]
        public Category category { get; set; }
        [JsonProperty(PropertyName = "description")]
        public string description { get; set; }
        [JsonProperty(PropertyName = "publishedBy")]
        public PublisherInfo publishedBy { get; set; }
        [JsonProperty(PropertyName = "authors")]
        public List<AuthorInfo> authors { get; set; }
        [JsonProperty(PropertyName = "publishedAt")]
        [JsonConverter(typeof(DayConverter))]
        public DateTime publishedAt { get; set; }
        [JsonProperty(PropertyName = "createdAt")]
        public DateTime createdAt { get; set; }
        [JsonProperty(PropertyName = "updatedAt")]
        public DateTime updatedAt { get; set; }
    }
    #endregion
    #region BookQueryCondition
    [JsonObject("BookQueryCondition")]
    public class BookQueryCondition
    {
        [JsonProperty(PropertyName = "bookIdEq")]
        public Nullable<BookId> bookIdEq { get; set; }
        [JsonProperty(PropertyName = "bookIdIn")]
        public List<BookId> bookIdIn { get; set; }
        [JsonProperty(PropertyName = "isbnEq")]
        public ISBN isbnEq { get; set; }
        [JsonProperty(PropertyName = "categoryIn", ItemConverterType = typeof(StringEnumConverter))]
        public List<Category> categoryIn { get; set; }
        [JsonProperty(PropertyName = "authorNameLike")]
        public string authorNameLike { get; set; }
        [JsonProperty(PropertyName = "publisherNameLike")]
        public string publisherNameLike { get; set; }
        [JsonProperty(PropertyName = "publishedFrom")]
        [JsonConverter(typeof(DayConverter))]
        public DateTime? publishedFrom { get; set; }
        [JsonProperty(PropertyName = "publishedTo")]
        [JsonConverter(typeof(DayConverter))]
        public DateTime? publishedTo { get; set; }
    }
    #endregion
    #region BookList
    [JsonObject("BookList")]
    public class BookList
    {
        [JsonProperty(PropertyName = "hits")]
        public int hits { get; set; }
        [JsonProperty(PropertyName = "page")]
        public int page { get; set; }
        [JsonProperty(PropertyName = "per_page")]
        public int per_page { get; set; }
        [JsonProperty(PropertyName = "result")]
        public List<Book> result { get; set; }
    }
    #endregion
}
