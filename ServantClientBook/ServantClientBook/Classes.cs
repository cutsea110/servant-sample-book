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
        [JsonProperty("addressId")]
        public int? addressId { get; set; }
        [JsonProperty("postcode")]
        public string postcode { get; set; }
        [JsonProperty("prefecture")]
        [JsonConverter(typeof(StringEnumConverter))]
        public Prefecture prefecture { get; set; }
        [JsonProperty("address")]
        public string address { get; set; }
        [JsonProperty("building")]
        public string building { get; set; }
        [JsonProperty("tel")]
        public string tel { get; set; }
        [JsonProperty("fax")]
        public string fax { get; set; }
        [JsonProperty("email")]
        public string email { get; set; }
        [JsonProperty("createdAt")]
        public DateTime createdAt { get; set; }
        [JsonProperty("updatedAt")]
        public DateTime updatedAt { get; set; }
    }
    #endregion
    #region Author
    [JsonObject("Author")]
    public class Author
    {
        [JsonProperty("authorId")]
        public int? authorId { get; set; }
        [JsonProperty("name")]
        public string name { get; set; }
        [JsonProperty("gender")]
        [JsonConverter(typeof(StringEnumConverter))]
        public Gender gender { get; set; }
        [JsonProperty("birth")]
        [JsonConverter(typeof(DayConverter))]
        public DateTime birth { get; set; }
        [JsonProperty("age")]
        public int age { get; set; }
        [JsonProperty("address")]
        public Address address { get; set; }
        [JsonProperty("createdAt")]
        public DateTime createdAt { get; set; }
        [JsonProperty("updatedAt")]
        public DateTime updatedAt { get; set; }
    }
    #endregion
    #region Publisher
    [JsonObject("Publisher")]
    public class Publisher
    {
        [JsonProperty("publisherId")]
        public int? publisherId { get; set; }
        [JsonProperty("name")]
        public string name { get; set; }
        [JsonProperty("companyType")]
        [JsonConverter(typeof(StringEnumConverter))]
        public CompanyType companyType { get; set; }
        [JsonProperty("address")]
        public Address address { get; set; }
        [JsonProperty("createdAt")]
        public DateTime createdAt { get; set; }
        [JsonProperty("updatedAt")]
        public DateTime updatedAt { get; set; }
    }
    #endregion
    #region Book
    [JsonObject("AuthorInfo")]
    public class AuthorInfo
    {
        [JsonProperty("authorId")]
        public int authorId { get; set; }

        [JsonProperty("name")]
        public string name { get; set; }
    }
    [JsonObject("PublisherInfo")]
    public class PublisherInfo
    {
        [JsonProperty("publisherId")]
        public int publisherId { get; set; }
        [JsonProperty("name")]
        public string name { get; set; }
    }

    [JsonObject("Book")]
    public class Book
    {
        [JsonProperty("bookId")]
        public int? bookId { get; set; }
        [JsonProperty("title")]
        public string title { get; set; }
        [JsonProperty("isbn")]
        public string isbn { get; set; }
        [JsonProperty("category")]
        [JsonConverter(typeof(StringEnumConverter))]
        public Category category { get; set; }
        [JsonProperty("description")]
        public string description { get; set; }
        [JsonProperty("publishedBy")]
        public PublisherInfo publishedBy { get; set; }
        [JsonProperty("authors")]
        public List<AuthorInfo> authors { get; set; }
        [JsonProperty("publishedAt")]
        [JsonConverter(typeof(DayConverter))]
        public DateTime publishedAt { get; set; }
        [JsonProperty("createdAt")]
        public DateTime createdAt { get; set; }
        [JsonProperty("updatedAt")]
        public DateTime updatedAt { get; set; }
    }
    #endregion
    #region BookQuery
    [JsonObject("BookQuery")]
    public class BookQuery
    {
        [JsonProperty("bookIdEq")]
        public int? bookIdEq { get; set; }
        [JsonProperty("bookIdIn")]
        public List<int> bookIdIn { get; set; }
        [JsonProperty("isbnEq")]
        public string isbnEq { get; set; }
        [JsonProperty("categoryIn", ItemConverterType = typeof(StringEnumConverter))]
        public List<Category> categoryIn { get; set; }
        [JsonProperty("authorNameLike")]
        public string authorNameLike { get; set; }
        [JsonProperty("publisherNameLike")]
        public string publisherNameLike { get; set; }
        [JsonProperty("publishedFrom")]
        [JsonConverter(typeof(DayConverter))]
        public DateTime? publishedFrom { get; set; }
        [JsonProperty("publishedTo")]
        [JsonConverter(typeof(DayConverter))]
        public DateTime? publishedTo { get; set; }
    }
    #endregion
    #region BookList
    [JsonObject("BookList")]
    public class BookList
    {
        [JsonProperty("hits")]
        public int hits { get; set; }
        [JsonProperty("page")]
        public int page { get; set; }
        [JsonProperty("per_page")]
        public int per_page { get; set; }
        [JsonProperty("result")]
        public List<Book> result { get; set; }
    }
    #endregion
}
