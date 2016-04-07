using Newtonsoft.Json;
using Newtonsoft.Json.Converters;
using System;
using System.Collections.Generic;

namespace ServantClientBook
{
    #region Address
    [JsonObject("Address")]
    public class Address
    {
        [JsonProperty(PropertyName = "addressId")]
        public int? addressId { get; set; }
        [JsonProperty(PropertyName = "postcode")]
        public string postcode { get; set; }
        [JsonProperty(PropertyName = "prefecture")]
        [JsonConverter(typeof(StringEnumConverter))]
        public Prefecture prefecture { get; set; }
        [JsonProperty(PropertyName = "address")]
        public string address { get; set; }
        [JsonProperty(PropertyName = "building")]
        public string building { get; set; }
        [JsonProperty(PropertyName = "tel")]
        public string tel { get; set; }
        [JsonProperty(PropertyName = "fax")]
        public string fax { get; set; }
        [JsonProperty(PropertyName = "email")]
        public string email { get; set; }
        [JsonProperty(PropertyName = "createdAt")]
        public DateTime createdAt { get; set; }
        [JsonProperty(PropertyName = "updatedAt")]
        public DateTime updatedAt { get; set; }
    }
    #endregion
    #region Author
    [JsonObject("Author")]
    public class Author
    {
        [JsonProperty(PropertyName = "authorId")]
        public int? authorId { get; set; }
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
    [JsonObject("AuthorQueryCondition")]
    public class AuthorQueryCondition
    {
        [JsonProperty("authorNameLike")]
        public string authorNameLike { get; set; }
        [JsonProperty("genderEq")]
        [JsonConverter(typeof(StringEnumConverter))]
        public Gender? genderEq { get; set; }
        [JsonProperty("ageFrom")]
        public int? ageFrom { get; set; }
        [JsonProperty("ageTo")]
        public int? ageTo { get; set; }
        [JsonProperty("prefectureIn", ItemConverterType = typeof(StringEnumConverter))]
        public List<Prefecture> prefectureIn { get; set; }
    }
    [JsonObject("AuthorList")]
    public class AuthorList
    {
        [JsonProperty("hits")]
        public int hits { get; set; }
        [JsonProperty("page")]
        public int page { get; set; }
        [JsonProperty("per_page")]
        public int per_page { get; set; }
        [JsonProperty("result")]
        public List<Author> result { get; set; }
    }
    #endregion
    #region Publisher
    [JsonObject("Publisher")]
    public class Publisher
    {
        [JsonProperty(PropertyName = "publisherId")]
        public int? publisherId { get; set; }
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
    #region Book
    [JsonObject("AuthorInfo")]
    public class AuthorInfo
    {
        [JsonProperty(PropertyName = "authorId")]
        public int authorId { get; set; }

        [JsonProperty(PropertyName = "name")]
        public string name { get; set; }
    }
    [JsonObject("PublisherInfo")]
    public class PublisherInfo
    {
        [JsonProperty(PropertyName = "publisherId")]
        public int publisherId { get; set; }
        [JsonProperty(PropertyName = "name")]
        public string name { get; set; }
    }

    [JsonObject("Book")]
    public class Book
    {
        [JsonProperty(PropertyName = "bookId")]
        public int? bookId { get; set; }
        [JsonProperty(PropertyName = "title")]
        public string title { get; set; }
        [JsonProperty(PropertyName = "isbn")]
        public string isbn { get; set; }
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
        public int? bookIdEq { get; set; }
        [JsonProperty(PropertyName = "bookIdIn")]
        public List<int> bookIdIn { get; set; }
        [JsonProperty(PropertyName = "isbnEq")]
        public string isbnEq { get; set; }
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
