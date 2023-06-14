#if !defined(MK_CT_STRING_HPP)
#define MK_CT_STRING_HPP

#include <cstddef>
#include <string>
#include <type_traits>
#include <ostream>
#include <algorithm>

namespace mk {

    namespace detail {
        
        template<typename CharT, typename T>
        struct is_std_string : std::false_type {};

        template<typename CharT, class Traits, class Allocator>
        struct is_std_string<CharT, std::basic_string<CharT, Traits, Allocator>> : std::true_type {};

        template<typename CharT, class Traits>
        struct is_std_string<CharT, std::basic_string_view<CharT, Traits>> : std::true_type {};

        template<typename CharT, typename T>
        concept std_string = is_std_string<CharT, std::decay_t<T>>::value || requires(T t) {
            { t.data() } -> std::convertible_to<CharT const*>;
            { t.size() } -> std::convertible_to<std::size_t>;
        };

    } // namespace detail
    
    template<typename CharT, std::size_t N>
    struct BasicCtString {
        using value_type = CharT;
        using size_type = std::size_t;
        using difference_type = std::ptrdiff_t;
        using const_reference = CharT const&;
        using const_pointer = CharT const*;
        using const_iterator = CharT const*;

        constexpr BasicCtString(const CharT (&str)[N]) noexcept
            : m_data{}
        {
            std::copy_n(str, m_data.size(), m_data.begin());
        }
        
        constexpr BasicCtString(const BasicCtString&) noexcept = default;
        constexpr BasicCtString(BasicCtString&&) noexcept = default;
        constexpr BasicCtString& operator=(const BasicCtString&) noexcept = default;
        constexpr BasicCtString& operator=(BasicCtString&&) noexcept = default;
        constexpr ~BasicCtString() noexcept = default;

        [[nodiscard]] constexpr const_iterator begin() const noexcept {
            return m_data.begin();
        }

        [[nodiscard]] constexpr const_iterator end() const noexcept {
            return m_data.end();
        }

        [[nodiscard]] constexpr const_reference operator[](size_type pos) const noexcept {
            return m_data[pos];
        }

        [[nodiscard]] constexpr size_type size() const noexcept {
            return m_data.size();
        }

        [[nodiscard]] constexpr const_pointer data() const noexcept {
            return m_data.data();
        }

        template<typename T>
            requires detail::std_string<CharT, T>
        [[nodiscard]] friend constexpr bool operator==(BasicCtString const& self, T&& other) noexcept {
            return self.size() == other.size() && std::equal(self.begin(), self.end(), other.data());
        }
        
        template<typename T>
            requires detail::std_string<CharT, T>
        [[nodiscard]] friend constexpr bool operator!=(BasicCtString const& self, T&& other) noexcept {
            return !(self == other);
        }

        [[nodiscard]] friend constexpr bool operator==(BasicCtString const& self, char const* str) noexcept {
            return std::string_view(self.data(), self.size()) == std::string_view(str);
        }
        
        [[nodiscard]] friend constexpr bool operator!=(BasicCtString const& self, char const* str) noexcept {
            return !(self == str);
        }

        constexpr auto view(size_type pos = 0, size_type count = std::string::npos) const noexcept {
            return std::basic_string_view<CharT>(data() + pos, count);
        }

        friend std::ostream& operator<<(std::ostream& os, BasicCtString const& str) {
            return os << std::string_view(str.data(), str.size());
        }

        std::array<CharT, N - 1> m_data;
    };

    template<typename CharT, std::size_t N>
    BasicCtString(const CharT (&)[N]) -> BasicCtString<CharT, N>;
    

    template<std::size_t N>
    struct CtString: BasicCtString<char, N> {
        using BasicCtString<char, N>::BasicCtString;
        
        template<typename... Args>
        constexpr CtString(Args&&... args) noexcept
            : BasicCtString<char, N>(std::forward<Args>(args)...)
        {}
    };

    template<std::size_t N>
    CtString(const char (&)[N]) -> CtString<N>;

    template<typename CharT>
    CtString(std::basic_string_view<CharT>) -> CtString<0>;

} // namespace mk


#endif // MK_CT_STRING_HPP
